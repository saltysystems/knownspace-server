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
-define(DEFAULT_BOUNDARY, 2500).
% Set the default buffer depth in milliseconds
-define(DEFAULT_BUFFER_DEPTH, 1000).

%% API

% Overworld RPCs
-define(KS_ZONE_JOIN, 16#2001).
-define(KS_ZONE_PART, 16#2002).
-define(KS_ZONE_INPUT, 16#2003).
-define(KS_ZONE_XFER, 16#2004).
-define(KS_ZONE_NEW_ENTITY, 16#2010).

rpc_info() ->
    [
        #{
            opcode => ?KS_ZONE_JOIN,
            c2s_handler => {?MODULE, join, 2},
            s2c_call => join,
            encoder => ks_pb
        },
        #{
            opcode => ?KS_ZONE_PART,
            c2s_handler => {?MODULE, part, 1},
            s2c_call => part,
            encoder => ks_pb
        },
        #{
            opcode => ?KS_ZONE_INPUT,
            c2s_handler => {?MODULE, input, 2},
            encoder => ks_pb
        },
        #{
            opcode => ?KS_ZONE_XFER,
            s2c_call => state_transfer,
            encoder => ks_pb
        },
        #{
            opcode => ?KS_ZONE_NEW_ENTITY,
            s2c_call => new_entity,
            encoder => ks_pb
        }
    ].

%%%====================================================================
%%% Structures
%%%====================================================================
-record(gamestate, {
    entities = [] :: list(),
    projectiles = [] :: list(),
    timestamp :: integer()
}).
-type gamestate() :: #gamestate{}.

-record(phys, {
    pos :: overworld_vector:vector(),
    vel :: overworld_vector:vector(),
    rot :: number()
}).
-type phys() :: #phys{}.

-record(entity, {
    id :: integer(),
    type :: non_neg_integer(),
    name :: string(),
    phys :: phys(),
    hitbox :: [overworld_vector:vector(), ...],
    stats :: any(),
    latency :: number()
}).
-type entity() :: #entity{}.

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
    ow_zone:rpc(?SERVER, input, Msg, Session).

%%%====================================================================
%%% Callbacks
%%%====================================================================
init([]) ->
    % Create an empty, new gamestate
    GameState = #gamestate{
        timestamp = erlang:system_time(millisecond)
    },
    % Initialize the zone with empty buffers and some default parameters
    InitialZoneState =
        #{
            input_buffer => [],
            gamestate_buffer => [GameState],
            % milliseconds
            buffer_depth => ?DEFAULT_BUFFER_DEPTH,
            % radial
            boundary => ?DEFAULT_BOUNDARY
        },
    {ok, InitialZoneState}.

% TODO: This isn't matching when gamestate buffer is an empty list.
% Maybe don't trim the buffer by time, but by entry count so there's always
% some gamestate to be checked against.
handle_join(Msg, Session, State = #{gamestate_buffer := [GameState | _]}) ->
    ID = ow_session:get_id(Session),
    Handle = maps:get(handle, Msg),
    logger:notice("Player ~p:~p has joined the server!", [Handle, ID]),
    % Add the handle to the player info
    PlayerInfo = #{handle => Handle},
    % Add the entity to the current gamestate
    {Entity, _GameState1} = new_entity(Handle, ID, GameState),
    % Let everyone know that the Player has joined
    Reply = {'@zone', {new_entity, entity_map(Entity)}},
    {Reply, {ok, Session, PlayerInfo}, State}.

handle_part(Session, State) ->
    ID = ow_session:get_id(Session),
    Player = ow_player_reg:get(ID),
    Handle = maps:get(handle, ow_player_reg:get_info(Player)),
    logger:notice("Player ~p:~p has left the server!", [Handle, ID]),
    % Let everyone know that the Player departed
    Msg = #{id => ID},
    Reply = {'@zone', {part, Msg}},
    {Reply, ok, State}.

handle_rpc(input, Msg, Session, State = #{input_buffer := InputBuffer}) ->
    % Inject the ID of the player moving into the Msg
    ID = ow_session:get_id(Session),
    Latency = ow_session:get_latency(Session),
    Msg1 = Msg#{id => ID, lag => Latency},
    % Add the action to the input queue
    State1 = State#{input_buffer => [Msg1 | InputBuffer]},
    {noreply, ok, State1}.

handle_tick(#{tick_rate := TRate}, State) ->
    State1 = update_gamestate(TRate, State),
    ToXfer = #{
        actions => [],
        projectiles => [],
        collisions => []
    },
    Reply = {'@zone', {state_transfer, ToXfer}},
    {Reply, State1}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================
update_gamestate(TickRate, State0) ->
    % Get the current game state and inputs
    #{gamestate_buffer := GameStateBuffer, input_buffer := Inputs, buffer_depth := BufDepth} =
        State0,
    [CurGameState | _] = GameStateBuffer,
    % Apply *inputs* received from players this tick
    CurGameState1 = apply_inputs(Inputs, CurGameState),
    % Apply physics to their respective entities
    CurGameState2 = update_positions(TickRate, CurGameState1),
    % Update the timestamp
    CurGameState3 = CurGameState2#gamestate{
        timestamp = erlang:system_time()
    },
    % Update the buffer with the final gamestate for this frame
    GameStateBuffer1 = [CurGameState3 | GameStateBuffer],
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

%----------------------------------------------------------------------
% Entity-specific Internal Functions
%----------------------------------------------------------------------
apply_inputs([], GameState0) ->
    GameState0;
apply_inputs([Input | Rest], GameState0) ->
    % This will have a list of maps containing the actual keysequence, ID, and
    % latency
    #{id := ID, keys := Keys} = Input,
    GameState1 = apply_input_per_player(ID, Keys, GameState0),
    apply_inputs(Rest, GameState1).

apply_input_per_player(ID, Keys, GameState0) ->
    % We check every key that is applied during the frame
    % and apply the appropriate physics input
    ApplyInput =
        fun(KeyPress, GameState) ->
            Entity = entity_by_id(ID, GameState),
            Actions = action_table(),
            case lists:keyfind(KeyPress, 1, Actions) of
                {KeyPress, Fun} ->
                    E1 = Fun(Entity),
                    update_entity(E1, GameState);
                false ->
                    logger:debug("Client sent an unknown action: ~p", [KeyPress]),
                    GameState
            end
        end,
    lists:foldl(ApplyInput, GameState0, Keys).

action_table() ->
    [
        {'IMPULSE', fun(E) ->
            Phys = E#entity.phys,
            #{speed_fac := Speed} = E#entity.stats,
            % Get the current velocity
            {_Pos, {Xv, Yv}, Rot} = phys_to_tuple(Phys),
            % 1 keypress = Vector2(0,-1) * Speed
            Vel1 = {Xv + 0 * Speed, Yv - 1 * Speed},
            % Rotate the keyed vector by the current rotation
            {Xv2, Yv2} = ow_vector:rotate(Vel1, Rot),
            % Subtract (negative Y is up) from the original vector
            Vel3 = {Xv - Xv2, Yv - Yv2},
            % Update the entity
            E#entity{phys = Phys#{vel => Vel3}}
        end},
        {'ROTATION_LEFT', fun(E) ->
            Phys = E#entity.phys,
            {_Pos, _Vel, Rot} = phys_to_tuple(Phys),
            #{rotation_fac := RFactor} = E#entity.stats,
            Rot1 = Rot - RFactor,
            E#entity{phys = Phys#{rot => Rot1}}
        end},
        {'ROTATION_RIGHT', fun(E) ->
            Phys = E#entity.phys,
            {_Pos, _Vel, Rot} = phys_to_tuple(Phys),
            #{rotation_fac := RFactor} = E#entity.stats,
            Rot1 = Rot + RFactor,
            E#entity{phys = Phys#{rot => Rot1}}
        end}
    ].

%apply_phys_intent(ID, Phys, GameState) ->
%    % This function will accept any new input received from the player session
%    % and update the server appropriately.  We will start by applying new
%    % Velocity vectors received from the client, but what we really ought to do
%    % is accept only input commands and use those to calculate new
%    % velocity/rotation for the entity. TODO.
%    {_Pos, Vel, Rot} = phys_to_tuple(Phys),
%    EntityInfo = entity_by_id(ID, GameState),
%    % Zero out any tiny velocities to hopefully help with floating point
%    % rounding issues.
%    NewVel =
%        case overworld_vector:length_squared(Vel) < 0.01 of
%           false ->
%               Vel;
%           true ->
%               {0, 0}
%        end,
%    % Check to see if the player is moving an entity that doesn't exist
%    % Otherwise continue to update the phys
%    Entity2 = EntityInfo#entity{
%        vel = NewVel,
%        rot = Rot
%    },
%    update_entity(Entity2, GameState).

-spec update_positions(integer(), gamestate()) -> gamestate().
update_positions(TickRate, GS) ->
    % We only do simulation only for entities that can change their velocity
    % vector. It is assumed all bullets etc undergo simple projectile motion so
    % we just let the client do that calculation locally, but we do check for
    % collisions server side.
    % If you want to run simulation for projectiles, you'll need to update this
    % function to understand other record types.
    Entities = GS#gamestate.entities,
    % Run the simulation for TickRate
    PositionFun = fun(E) ->
        Phys = E#entity.phys,
        #{pos := {Xp, Yp}, vel := {Xv, Yv}} = Phys,
        TickMs = TickRate / 1000,
        %Latency = get_latency(E),
        % TODO: Re-add latency - just call it from the session
        Latency = 0,
        NewPos = {
            Xp + (Xv * (TickMs + Latency)),
            Yp + (Yv * (TickMs + Latency))
        },
        E#entity{phys = Phys#{pos => NewPos}}
    end,
    NewEnts = [PositionFun(X) || X <- Entities],
    %case NewEnts of
    %    [] -> ok;
    %    _ ->
    %        io:format("New positions: ~p~n", [NewEnts])
    %end,
    GS#gamestate{entities = NewEnts}.

new_entity(Handle, ID, GameState) ->
    Entities0 = GameState#gamestate.entities,
    Entity = #entity{
        id = ID,
        name = Handle,
        phys =
            #{
                pos => ow_vector:vector_map({0, 0}),
                vel => ow_vector:vector_map({0, 0}),
                rot => 0
            },
        hitbox = ow_vector:rect_to_maps([
            {-20, -20},
            {-20, 20},
            {20, -20},
            {20, 20}
        ]),
        stats =
            #{
                max_hp => 100,
                cur_hp => 100,
                % rotation / rotation_factor
                rotation_fac => 20,
                speed_fac => 5
            }
    },
    GameState1 = GameState#gamestate{entities = [Entity | Entities0]},
    {Entity, GameState1}.

entity_by_id(ID, GameState) ->
    EList = GameState#gamestate.entities,
    lists:keyfind(ID, #entity.id, EList).

update_entity(Entity, GameState) ->
    ID = Entity#entity.id,
    EList = GameState#gamestate.entities,
    lists:keystore(ID, #entity.id, EList, Entity).

phys_to_tuple(#{pos := #{x := Xp, y := Yp}, vel := #{x := Xv, y := Yv}, rot := Rot}) ->
    Pos = {Xp, Yp},
    Vel = {Xv, Yv},
    {Pos, Vel, Rot}.

-spec entity_map(entity()) -> map().
entity_map(Entity) ->
    #{
        id => Entity#entity.id,
        name => Entity#entity.name,
        phys => Entity#entity.phys,
        hitbox => Entity#entity.hitbox,
        stats => Entity#entity.stats
    }.
