-module(ks_zone).
-behaviour(ow_zone).

-export([
    init/1,
    handle_join/3,
    handle_part/2,
    handle_rpc/4,
    handle_tick/2,
    rpc_info/0
]).

-export([
    start/0,
    stop/0,
    join/2,
    part/1,
    input/2
]).

% Allow instantiating multiple instances of zones
%-define(SERVER(Name), {via, gproc, {n, l, {?MODULE, Name}}}).
-define(SERVER, ?MODULE).
% Radial boundary size
-define(DEFAULT_BOUNDARY, 1024).
% Set the default buffer depth in milliseconds
-define(DEFAULT_BUFFER_DEPTH, 500).

%% API

% Overworld RPCs
-define(KS_ZONE_JOIN, 16#2001).
-define(KS_ZONE_PART, 16#2002).
-define(KS_ZONE_INPUT, 16#2003).
-define(KS_ZONE_XFER, 16#2004).
-define(KS_ZONE_SNAP, 16#2005).
-define(KS_ZONE_ENTITY, 16#2010).

rpc_info() ->
    [
        #{
            opcode => ?KS_ZONE_JOIN,
            c2s_handler => {?MODULE, join, 2},
            s2c_call => join,
            encoder => ks_pb,
            qos => reliable,
            channel => 0
        },
        #{
            opcode => ?KS_ZONE_PART,
            c2s_handler => {?MODULE, part, 1},
            s2c_call => part,
            encoder => ks_pb,
            qos => reliable,
            channel => 0
        },
        #{
            opcode => ?KS_ZONE_INPUT,
            c2s_handler => {?MODULE, input, 2},
            encoder => ks_pb,
            qos => reliable,
            channel => 0
        },
        #{
            opcode => ?KS_ZONE_XFER,
            s2c_call => zone_transfer,
            encoder => ks_pb,
            qos => reliable,
            channel => 0
        },
        #{
            opcode => ?KS_ZONE_SNAP,
            s2c_call => zone_snapshot,
            encoder => ks_pb,
            qos => unsequenced,
            channel => 1
        },
        #{
            opcode => ?KS_ZONE_ENTITY,
            s2c_call => entity,
            encoder => ks_pb,
            qos => reliable,
            channel => 0
        }
    ].

%%%====================================================================
%%% Structures and Types
%%%====================================================================
-type direction() :: fwd | rev | left | right.
-type move() :: impulse | rotate.
-type vector() :: ow_vector:vector().
-type vector_map() :: #{
    x => number(),
    y => number()
}.
-type zone_boundary() :: number() | {number(), number(), number(), number()}.

-record(gamestate, {
    entities = [] :: list(),
    projectiles = [] :: list(),
    new_projectiles = [] :: list(),
    collisions = [] :: list(),
    timestamp :: integer()
}).
-type gamestate() :: #gamestate{}.

-type phys() :: #{
    pos => vector_map(),
    vel => vector_map(),
    rot => number()
}.

-type entity() :: #{
    id => integer(),
    % not sent
    type => non_neg_integer(),
    name => string(),
    phys => phys(),
    hitbox => [vector_map(), ...],
    stats => any(),
    % not sent
    latency => number()
}.

-type projectile() :: #{
    id => integer(),
    owner => integer(),
    type => non_neg_integer(),
    phys => phys(),
    hitbox => [vector_map(), ...],
    stats => any(),
    ttl => non_neg_integer(),
    create_time => integer()
}.

%%%====================================================================
%%% API
%%%====================================================================

start() ->
    ow_zone:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    ow_zone:stop(?SERVER).

join(Msg, Session) ->
    ow_zone:join(?SERVER, Msg, Session).

part(Session) ->
    ow_zone:part(?SERVER, Session).

input(Msg, Session) ->
    case Msg of
        <<>> ->
            % no input
            ok;
        Msg ->
            ow_zone:rpc(?SERVER, input, Msg, Session)
    end.

%%%====================================================================
%%% Callbacks
%%%====================================================================
init([]) ->
    % Create an empty, new gamestate
    GameState = #gamestate{
        timestamp = erlang:system_time(millisecond)
    },
    % Initialize the zone with empty buffers and some default parameters
    ECSWorld = zone,
    InitialZoneState =
        #{
            input_buffer => [],
            gamestate_buffer => [GameState],
            % milliseconds
            buffer_depth => ?DEFAULT_BUFFER_DEPTH,
            % radial
            boundary => ?DEFAULT_BOUNDARY,
            ecs_world => ECSWorld
        },
    Config = #{tick_ms => 50},
    % Start the ECS server. This should be handled by a supervisor
    ow_ecs:start_link(ECSWorld),
    {ok, InitialZoneState, Config}.

handle_join(Msg, Session, State = #{gamestate_buffer := [GameState | Rest]}) ->
    ID = ow_session:get_id(Session),
    Handle = maps:get(handle, Msg),
    logger:notice("Player ~p:~p has joined the server!", [Handle, ID]),
    % Add the handle to the player info
    PlayerInfo = #{handle => Handle},
    % Add the entity to the current gamestate
    {Entity, GameState1} = new_entity(Handle, ID, GameState),
    % Let everyone else know that the Player has joined
    ow_zone:broadcast(self(), {entity, entity_map(Entity)}),
    % Swap the head of the buffer with the new version of the GameState with
    % the new player injected.
    GameStateBuffer = [GameState1 | Rest],
    State1 = State#{gamestate_buffer := GameStateBuffer},
    % Build a reply to the player with information about entities who joined
    % before them.
    ZoneXfer = #{
        entities => get_entities(GameState),
        projectiles => all_projectiles_map(GameState)
    },
    Reply = {{'@', [ID]}, {zone_transfer, ZoneXfer}},
    % Reply to the player, update the player registry and the zone state
    {Reply, {ok, Session, PlayerInfo}, State1}.

handle_part(Session, State = #{gamestate_buffer := [GameState | Rest]}) ->
    ID = ow_session:get_id(Session),
    % Check if the player is still in the zone
    case ow_player_reg:get(ID) of
        % For now, just check to see if the player is connected. This will need
        % to be revisited when multizone comes online
        {error, _} ->
            % Not in the zone
            {noreply, ok, State};
        Player ->
            Handle = maps:get(handle, ow_player_reg:get_info(Player)),
            logger:notice("Player ~p:~p has left the server!", [Handle, ID]),
            % Remove the player from the entities list
            GameState1 = rm_entity(ID, GameState),
            GameStateBuffer = [GameState1 | Rest],
            State1 = State#{gamestate_buffer := GameStateBuffer},
            % Let everyone know that the Player departed
            Msg = #{id => ID},
            Reply = {'@zone', {part, Msg}},
            {Reply, ok, State1}
    end.

handle_rpc(input, Msg, Session, State = #{input_buffer := InputBuffer}) ->
    % Inject the ID of the player moving into the Msg
    ID = ow_session:get_id(Session),
    Latency = ow_session:get_latency(Session),
    Msg1 = Msg#{id => ID, latency => Latency},
    % Add the action to the input queue
    State1 = State#{input_buffer => [Msg1 | InputBuffer]},
    {noreply, ok, State1}.

handle_tick(TickMs, State) ->
    %Last = maps:get(last_tick, State, TickMs),
    %Now = erlang:monotonic_time(),
    %Delta = erlang:convert_time_unit(Now - Last, native, millisecond),
    State1 = update_gamestate(TickMs, State),
    #{gamestate_buffer := [GameState | _Rest]} = State1,
    ToXfer = #{
        phys_updates => get_entity_phys(GameState),
        projectiles => new_projectiles_map(GameState),
        collisions => new_collision_map(GameState)
    },
    Reply = {'@zone', {zone_snapshot, ToXfer}},
    {Reply, State1}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

update_gamestate(TickRate, State0) ->
    % Get the current game state and inputs
    #{
        gamestate_buffer := GameStateBuffer,
        input_buffer := Inputs,
        buffer_depth := BufDepth,
        boundary := Boundary
    } = State0,
    [GameState | _] = GameStateBuffer,
    % Move any new projectiles into the projectile list and clear the queue
    % also delete any collisions for the new gamestate
    CurGameState0 = refresh_gamestate(GameState),
    % Apply *inputs* received from players this tick
    CurGameState1 = apply_inputs(Inputs, CurGameState0),
    % Apply physics to their respective entities
    CurGameState2 = update_positions(TickRate, CurGameState1),
    CurGameState3 = apply_collisions(Boundary, CurGameState2),
    % Update the timestamp
    CurGameState4 = CurGameState3#gamestate{
        timestamp = erlang:system_time()
    },
    % Trim any projectiles that have lived too long
    CurGameState5 = trim_projectiles(CurGameState4),
    % Update the buffer with the final gamestate for this frame
    GameStateBuffer1 = [CurGameState5 | GameStateBuffer],
    % Trim off any game states we no longer care about
    GameStateBuffer2 = trim_gamestate_buffer(BufDepth, GameStateBuffer1),
    State0#{
        gamestate_buffer := GameStateBuffer2,
        % emptied out after applying inputs
        input_buffer := []
    }.

trim_gamestate_buffer(BufDepth, GameStates) ->
    Now = erlang:system_time(),
    Predicate = fun(Item) ->
        SnapshotTime = Item#gamestate.timestamp,
        Delta = Now - SnapshotTime,
        T = erlang:convert_time_unit(Delta, native, millisecond),
        T =< BufDepth
    end,
    lists:filter(Predicate, GameStates).

refresh_gamestate(GameState) ->
    GameState1 = dequeue_projectiles(GameState),
    GameState1#gamestate{collisions = []}.

%----------------------------------------------------------------------
% Data serialization functions
%----------------------------------------------------------------------
new_collision_map(#gamestate{collisions = Collisions}) ->
    Fun =
        fun({Obj1, Obj2}, AccIn) ->
            ID1 = maps:get(id, Obj1),
            ID2 = maps:get(id, Obj2),
            [#{id => [ID1, ID2]} | AccIn]
        end,
    L = lists:foldl(Fun, [], Collisions),
    %io:format("Collision map list: ~p~n", [L]),
    L.

%----------------------------------------------------------------------
% Entity-specific Internal Functions
%----------------------------------------------------------------------
-spec apply_inputs(list(), gamestate()) -> gamestate().
apply_inputs([], GameState0) ->
    GameState0;
apply_inputs([Input | Rest], GameState0) ->
    % This will have a list of maps containing the actual keysequence, ID, and
    % latency
    #{id := ID, keys := Keys, latency := Latency} = Input,
    % The cursor position is an optional submessage, so give it some
    % semi-sensible default of the origin
    Cursor = maps:get(cursor, Input, ow_vector:vector_map({0, 0})),
    % It's possible, apparently, to get into a state where the player is gone
    % but the entity record hasn't been yet removed
    case entity_by_id(ID, GameState0) of
        false ->
            GameState0;
        _ ->
            GameState1 = update_latency(ID, Latency, GameState0),
            GameState2 = apply_input_per_player(ID, Keys, Cursor, GameState1),
            apply_inputs(Rest, GameState2)
    end.

apply_input_per_player(ID, Keys, Cursor, GameState0) ->
    % We check every key that is applied during the frame
    % and apply the appropriate physics input
    ApplyInput =
        fun(KeyPress, GameState) ->
            Entity = entity_by_id(ID, GameState),
            Actions = action_table(),
            case lists:keyfind(KeyPress, 1, Actions) of
                {KeyPress, Fun} ->
                    % Apply the key press and return the updated gamestate
                    Fun(Entity, Cursor, GameState);
                false ->
                    logger:debug("Client sent an unknown action: ~p", [KeyPress]),
                    GameState
            end
        end,
    lists:foldl(ApplyInput, GameState0, Keys).

update_latency(ID, Latency, GameState0) ->
    E = entity_by_id(ID, GameState0),
    E1 = E#{latency := Latency},
    update_entity(E1, GameState0).

action_table() ->
    % For now, movement only considers the entity in isolation.  May need to
    % include the current gamestate if we wnat to have rigid body physics
    % interacting with other objects, e.g.
    [
        {'IMPULSE_FWD', fun(E, _, GS) -> apply_move(impulse, fwd, E, GS) end},
        {'IMPULSE_REV', fun(E, _, GS) -> apply_move(impulse, rev, E, GS) end},
        {'IMPULSE_LEFT', fun(E, _, GS) -> apply_move(impulse, left, E, GS) end},
        {'IMPULSE_RIGHT', fun(E, _, GS) -> apply_move(impulse, right, E, GS) end},
        {'ROTATE_LEFT', fun(E, _, GS) -> apply_move(rotate, left, E, GS) end},
        {'ROTATE_RIGHT', fun(E, _, GS) -> apply_move(rotate, right, E, GS) end},
        {'ACTION_0', fun(E, C, GS) -> create_projectile(E, C, GS) end}
    ].

-spec apply_move(move(), direction(), entity(), gamestate()) -> gamestate().
apply_move(Type, Direction, Entity, GameState) ->
    #{phys := Phys, stats := Stats} = Entity,
    NewPhys =
        case Type of
            impulse ->
                apply_impulse(Direction, Phys, Stats);
            rotate ->
                apply_rotation(Direction, Phys, Stats)
        end,
    E1 = Entity#{phys => NewPhys},
    update_entity(E1, GameState).

new_entity(Handle, ID, GameState) ->
    Entities0 = GameState#gamestate.entities,
    Entity = #{
        id => ID,
        name => Handle,
        type => 0,
        phys =>
            #{
                pos => ow_vector:vector_map({rand:uniform(1024), rand:uniform(1024)}),
                vel => ow_vector:vector_map({0, 0}),
                rot => 0
            },
        hitbox => ow_vector:rect_to_maps([
            {-20, -20},
            {-20, 20},
            {20, -20},
            {20, 20}
        ]),
        stats =>
            #{
                max_hp => 100,
                cur_hp => 100,
                % rotation / rotation_factor
                rotation_fac => 10,
                speed_fac => 5,
                max_vel => 400
            },
        latency => 0
    },
    GameState1 = GameState#gamestate{entities = [Entity | Entities0]},
    {Entity, GameState1}.

entity_by_id(ID, GameState) ->
    EList = GameState#gamestate.entities,
    ks_util:mapfind(ID, id, EList).

-spec update_entity(entity(), gamestate()) -> gamestate().
update_entity(Entity = #{id := ID}, GameState) ->
    EList = GameState#gamestate.entities,
    EList2 = ks_util:mapstore(ID, id, EList, Entity),
    GameState#gamestate{entities = EList2}.

-spec rm_entity(pos_integer(), gamestate()) -> gamestate().
rm_entity(ID, GameState) ->
    EList = GameState#gamestate.entities,
    EList2 = ks_util:mapdelete(ID, id, EList),
    GameState#gamestate{entities = EList2}.

-spec entity_map(entity()) -> entity().
entity_map(Entity) ->
    Entity.
%#{
%    id => Entity#entity.id,
%    name => Entity#entity.name,
%    phys => Entity#entity.phys,
%    hitbox => Entity#entity.hitbox,
%    stats => Entity#entity.stats
%}.

-spec get_entities(gamestate()) -> [map()].
get_entities(GameState) ->
    Entities = GameState#gamestate.entities,
    F = fun(E, Acc0) ->
        EM = entity_map(E),
        [EM | Acc0]
    end,
    lists:foldl(F, [], Entities).

-spec get_entity_phys(gamestate()) -> [map()].
get_entity_phys(GameState) ->
    Entities = GameState#gamestate.entities,
    F = fun(E, Acc0) ->
        EM = entity_map(E),
        %#{ id := ID, phys := Phys } = EM,
        [maps:with([id, phys], EM) | Acc0]
    end,
    EP = lists:foldl(F, [], Entities),
    EP.

%----------------------------------------------------------------------
% Projectiles
%----------------------------------------------------------------------
-spec create_projectile(entity(), vector_map(), gamestate()) -> gamestate().
create_projectile(Entity, Cursor, GameState) ->
    % Get the current properties of the entity
    %Owner = Entity#entity.id,
    %Phys = Entity#entity.phys,
    #{id := Owner, phys := Phys} = Entity,
    {Pos = {Xe, Ye}, _Vel, _Rot} = phys_to_tuple(Phys),
    #{x := Xc, y := Yc} = Cursor,
    % Draw a line between the cursor position and the entity position.  This is
    % the direction of the projectile.  Normalize this vector and then scale by
    % the speed
    Speed = 300,
    Direction = {Xc - Xe, Yc - Ye},
    % test the length of the direction. if it's ~0 , then make it non-zero so it can be normalized
    SanitizedDir =
        case Direction of
            {0.0, 0.0} -> {0.1, 0.1};
            {0, 0} -> {1, 1};
            _ -> Direction
        end,
    Vel = ow_vector:scale(ow_vector:normalize(SanitizedDir), Speed),
    % Create the projectile object with some defaults filled in
    Projectile =
        #{
            id => erlang:unique_integer(),
            owner => Owner,
            type => ks_pb:enum_value_by_symbol(projectile_type, 'BALLISTIC'),
            phys =>
                #{
                    pos => ow_vector:vector_map(Pos),
                    vel => ow_vector:vector_map(Vel),
                    rot => 0
                },
            hitbox => ow_vector:rect_to_maps([
                {-10, -10},
                {-10, 10},
                {10, -10},
                {10, 10}
            ]),
            % Stats are internal, maybe use later
            stats => #{},
            ttl => 5000,
            create_time => erlang:system_time()
        },
    queue_projectile(Projectile, GameState).

-spec queue_projectile(projectile(), gamestate()) -> gamestate().
queue_projectile(Projectile, GameState) ->
    Projectiles = GameState#gamestate.new_projectiles,
    GameState#gamestate{new_projectiles = [Projectile | Projectiles]}.

-spec dequeue_projectiles(gamestate()) -> gamestate().
dequeue_projectiles(GameState) ->
    SentProjectiles = GameState#gamestate.new_projectiles,
    Projectiles = GameState#gamestate.projectiles,
    GameState#gamestate{
        new_projectiles = [],
        projectiles = SentProjectiles ++ Projectiles
    }.

-spec new_projectiles_map(gamestate()) -> list().
new_projectiles_map(GameState) ->
    ProjToSend = GameState#gamestate.new_projectiles,
    projectiles_map(ProjToSend).

-spec all_projectiles_map(gamestate()) -> list().
all_projectiles_map(GameState) ->
    NewProj = GameState#gamestate.new_projectiles,
    CurrentProj = GameState#gamestate.projectiles,
    ProjToSend = NewProj ++ CurrentProj,
    projectiles_map(ProjToSend).

-spec projectiles_map(list()) -> [map(), ...].
projectiles_map(Projectiles) ->
    Projectiles.
%ProjMap =
%    fun(P, AccIn) ->
%        PMap =
%            #{
%                owner => P#projectile.owner,
%                id => P#projectile.id,
%                phys => P#projectile.phys,
%                ttl => P#projectile.ttl,
%                hitbox => P#projectile.hitbox
%            },
%        [PMap | AccIn]
%    end,
%lists:foldl(ProjMap, [], Projectiles).

-spec trim_projectiles(gamestate()) -> gamestate().
trim_projectiles(GameState) ->
    Projectiles0 = GameState#gamestate.projectiles,
    Predicate = fun(Elem) ->
        Now = erlang:system_time(),
        #{create_time := CTime, ttl := TTL} = Elem,
        Delta = Now - CTime,
        DeltaMs = erlang:convert_time_unit(Delta, native, millisecond),
        DeltaMs =< TTL
    end,
    Projectiles1 = lists:filter(Predicate, Projectiles0),
    GameState#gamestate{projectiles = Projectiles1}.

%----------------------------------------------------------------------
% Game Physics
%----------------------------------------------------------------------
-spec apply_impulse(direction(), phys(), map()) -> phys().
apply_impulse(Direction, Phys, Stats) ->
    #{speed_fac := Speed, max_vel := MaxV} = Stats,
    % Get the current velocity
    {_Pos, {Xv, Yv}, Rot} = phys_to_tuple(Phys),
    % 1 keypress = Vector2(0,-1) * Speed
    Vel1 =
        case Direction of
            fwd ->
                {0 * Speed, 1 * Speed};
            rev ->
                {0 * Speed, -1 * Speed};
            left ->
                {-1 * Speed, 0 * Speed};
            right ->
                {1 * Speed, 0 * Speed}
        end,
    % Rotate the keyed vector by the current rotation
    {Xv2, Yv2} = ow_vector:rotate(Vel1, Rot),
    % Subtract (negative Y is up) from the original vector
    Vel3 = {Xv - Xv2, Yv - Yv2},
    % Check to see if the velocity exceeds max velocity
    Vel4 =
        case ow_vector:length_squared(Vel3) > math:pow(MaxV, 2) of
            true ->
                ow_vector:scale(ow_vector:normalize(Vel3), MaxV);
            false ->
                Vel3
        end,
    % Update the entity
    Phys#{vel => ow_vector:vector_map(Vel4)}.

-spec apply_rotation(left | right, phys(), map()) -> phys().
apply_rotation(Direction, Phys, Stats) ->
    {_Pos, _Vel, Rot} = phys_to_tuple(Phys),
    #{rotation_fac := RFactor} = Stats,
    D =
        case Direction of
            right -> 1.0;
            left -> -1.0
        end,
    Rot1 = Rot + (D / RFactor),
    Phys#{rot => Rot1}.

% TODO: Refactor this into a physics module
-spec update_positions(integer(), gamestate()) -> gamestate().
update_positions(TickRate, GS) ->
    % Run the simulation for TickRate
    Entities = GS#gamestate.entities,
    Projectiles = GS#gamestate.projectiles,
    PositionUpdate =
        fun(Phys) ->
            #{pos := PosMap, vel := VelMap} = Phys,
            #{x := Xv, y := Yv} = VelMap,
            #{x := Xp, y := Yp} = PosMap,
            DeltaT = TickRate / 1000,
            NewPos = {
                Xp + (Xv * DeltaT),
                Yp + (Yv * DeltaT)
            },
            Phys#{pos => ow_vector:vector_map(NewPos)}
        end,
    PhysFun =
        fun(E = #{phys := Phys}) ->
            E#{phys => PositionUpdate(Phys)}
        end,
    NewEnts = [PhysFun(X) || X <- Entities],
    NewProjs = [PhysFun(X) || X <- Projectiles],
    %case NewEnts of
    %    [] -> ok;
    %    _ ->
    %        logger:debug("New positions: ~p~n", [NewEnts])
    %end,
    GS#gamestate{entities = NewEnts, projectiles = NewProjs}.

%----------------------------------------------------------------------
% Collision Physics
%----------------------------------------------------------------------
-spec apply_collisions(zone_boundary(), gamestate()) -> gamestate().
apply_collisions(Boundary, GameState) ->
    Entities = GameState#gamestate.entities,
    Projectiles = GameState#gamestate.projectiles,
    % Get the collisions that happened during this frame
    Collisions = collisions(Boundary, Entities, Projectiles),
    GameState#gamestate{collisions = Collisions}.

-spec collisions(zone_boundary(), [entity(), ...], [projectile(), ...]) -> list().
collisions(Boundary, Entities, Projectiles) ->
    % Get the board parameters
    {Xmin, Ymin, Xmax, Ymax} =
        case Boundary of
            {X1, Y1, X2, Y2} ->
                {X1, Y1, X2, Y2};
            Radius ->
                % since any given side will be the diameter of the circle, just
                % set each value to the radius.
                {-Radius, -Radius, Radius, Radius}
        end,
    % Initialize the collision
    C0 = ow_collision:new(Xmin, Ymin, Xmax, Ymax),
    % Set up functions for getting the relevant position information
    PosFun =
        fun(E) ->
            #{phys := #{pos := Pos}} = E,
            ow_vector:vector_tuple(Pos)
        end,
    % Add the entities and objects to the quadtree
    C1 = ow_collision:add_entities(Entities, PosFun, C0),
    C2 = ow_collision:add_entities(Projectiles, PosFun, C1),
    % Get bounding box for entities or projectiles
    BBoxFun =
        fun(O) ->
            #{phys := #{pos := Pos}, hitbox := Hitbox} = O,
            HitboxTr = ow_vector:translate(Hitbox, Pos),
            % Convert the bounding box to a list of tuples
            ow_vector:rect_to_tuples(HitboxTr)
        end,
    Collisions0 = lists:flatten([area_entered(O, BBoxFun, C2) || O <- Entities]),
    % Filter out the collisions between a projectile and its owner.
    IsOwnerPred = fun({Obj1, Obj2}) ->
        not is_entity_owner(Obj1, Obj2)
    end,
    Collisions1 = lists:filter(IsOwnerPred, Collisions0),
    % Get rid of collisions where it's two entities. We don't know how to
    % handle that yet. TODO.
    NotBothEntities = fun({Obj1, Obj2}) ->
        is_entity(Obj1) and (not is_entity(Obj2)) xor
            is_entity(Obj2) and (not is_entity(Obj1))
    end,
    Collisions2 = lists:filter(NotBothEntities, Collisions1),
    % Filter out the duplicate pairs
    case Collisions2 of
        [] -> ok;
        _ -> logger:info("Collisions: ~p", [Collisions2])
    end,
    ow_util:remove_dups(Collisions2).
%% Return the updated GameState and list of collisions
%ApplyFun = fun(Collision, InitialState) ->
%    apply_collision(Collision, InitialState)
%end,
%lists:foldl(ApplyFun, GS, Collisions3).

area_entered(Object, BoundingBoxFun, QuadTree) ->
    % For every entity, check an area a bit beyond its position for potential
    % collisions. TODO: Does this need to be a function of tickrate, latency,
    % etc ?
    % TODO: Currently we are most interested in Entity<->Projectile or
    % Entity<->Entity collisions. Maybe we check Projectile<->[anything]
    % collisions? Too expensive?
    {X, Y} = get_position(Object),
    % Check a significant potential area around the entity. This is NOT the
    % collision detection, just the POTENTIAL FOR collisions. It's not clear
    % how to set these values to be reasonable for objects of dynamic size.
    Left = X - 25,
    Bottom = Y - 25,
    Right = X + 25,
    Top = Y + 25,
    Results = ow_collision:check_area(
        {Left, Bottom, Right, Top}, BoundingBoxFun, QuadTree
    ),
    [{O1, O2} || {O1, O2, Coll} <- Results, Coll == true].

%----------------------------------------------------------------------
% Misc. Internal Functions
%----------------------------------------------------------------------

-spec phys_to_tuple(map()) -> {vector(), vector(), integer()}.
phys_to_tuple(#{pos := #{x := Xp, y := Yp}, vel := #{x := Xv, y := Yv}, rot := Rot}) ->
    Pos = {Xp, Yp},
    Vel = {Xv, Yv},
    {Pos, Vel, Rot}.

-spec get_position(entity() | projectile()) -> {number(), number()}.
get_position(Object) ->
    #{phys := #{pos := Pos}} = Object,
    ow_vector:vector_tuple(Pos).

is_entity_owner(#{owner := Owner}, #{id := ID}) when ID == Owner ->
    true;
is_entity_owner(#{id := ID}, #{owner := Owner}) when ID == Owner ->
    true;
is_entity_owner(_, _) ->
    false.

-spec is_entity(any()) -> boolean().
% entities can't have owners
is_entity(#{owner := _ID}) ->
    false;
is_entity(_) ->
    true.
