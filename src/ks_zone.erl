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
         move/2
        ]).

% Allow instantiating multiple instances of zones
%-define(SERVER(Name), {via, gproc, {n, l, {?MODULE, Name}}}).
-define(SERVER, ?MODULE).
% Radial boundary size
-define(DEFAULT_BOUNDARY, 2500).
% Set the default buffer depth in milliseconds
-define(DEFAULT_BUFFER_DEPTH, 2000).

%% API 

% Overworld RPCs
-define(KS_ZONE_JOIN, 16#2001).
-define(KS_ZONE_PART, 16#2002).
-define(KS_ZONE_MOVE, 16#2003).
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
            opcode => ?KS_ZONE_MOVE,
            c2s_handler => {?MODULE, move, 2},
            c2s_proto => intent,
            encoder => ks_pb
        },
        #{
            opcode => ?KS_ZONE_PART,
            c2s_handler => {?MODULE, part, 1},
            s2c_call => part,
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

-record(entity, {
    id :: integer(),
    type :: non_neg_integer(),
    name :: string(),
    pos :: overworld_vector:vector(),
    vel :: overworld_vector:vector(),
    rot :: number(),
    hitbox :: [overworld_vector:vector(), ...],
    stats :: any(),
    latency :: number()
}).
%-type entity() :: #entity{}.


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

move(Msg, Session) ->
    ow_zone:rpc(?SERVER, move, Msg, Session).

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
            intent_buffer => [],
            gamestate_buffer => [GameState],
            buffer_depth => ?DEFAULT_BUFFER_DEPTH, % milliseconds
            boundary => ?DEFAULT_BOUNDARY % radial
        },
    {ok, InitialZoneState}.

handle_join(Msg, Session, State = #{ gamestate_buffer := [ GameState | _ ]}) ->
    ID = ow_session:get_id(Session),
    Handle = maps:get(handle, Msg),
    logger:notice("Player ~p:~p has joined the server!", [Handle,ID]),
    % Add the handle to the player info
    PlayerInfo = #{ handle => Handle },
    % Add the entity to the current gamestate
    {Entity, _GameState1} = new_entity(Handle, ID, GameState),
    % Let everyone know that the Player has joined
    Reply = {'@zone', {new_entity, Entity}},
    {Reply, {ok, Session, PlayerInfo}, State}.

handle_part(Session, State) ->
    ID = ow_session:get_id(Session),
    Player = ow_player_reg:get(ID),
    Handle = maps:get(handle, ow_player_reg:get_info(Player)),
    logger:notice("Player ~p:~p has left the server!", [Handle, ID]),
    % Let everyone know that the Player departed
    Msg = #{ id => ID },
    Reply = {'@zone', {part, Msg}},
    {Reply, ok, State}.

handle_rpc(move, Msg, Session, State = #{ intent_buffer := IntentBuffer0 }) ->
    % Inject the ID of the player moving into the Msg
    ID = ow_session:get_id(Session),
    Msg1 = Msg#{ id => ID },
    % Add the action to the intent queue
    State1 = State#{ intent_buffer => [ Msg1 | IntentBuffer0 ]},
    {noreply, ok, State1}.

handle_tick(#{ tick_rate := TRate }, State) ->
    State1 = process_intents(TRate, State),
    ToXfer = #{ actions => [], 
                projectiles => [], 
                collisions => [] 
              },
    Reply = {'@zone', {state_transfer, ToXfer}},
    {Reply, State1}.


%%%====================================================================
%%% Internal Functions
%%%====================================================================
process_intents(_, State0 = #{ gamestate_buffer := [] }) ->
    State0;
process_intents(TickRate, State0 = #{ buffer_depth := BufDepth } ) ->
    % Get the current game state
    GameStateBuffer = maps:get(gamestate_buffer, State0),
    [ CurrentGameState | _ ] = GameStateBuffer,
    % Get the queued intents
    Intents = maps:get(intent_buffer, State0),
    % Apply *inputs* received from players this tick
    CurrentGameState1 = apply_entity_intents(Intents, CurrentGameState),
    % Apply physics to their respective entities
    CurrentGameState2 = update_positions(TickRate, CurrentGameState1),
    % Update the buffer with the final gamestate for this frame
    GameStateBuffer1 = [ CurrentGameState2 | GameStateBuffer ],
    % Trim off any game states we no longer care about
    GameStateBuffer2 = trim_gamestate_buffer(BufDepth, GameStateBuffer1),
    State0#{ 
               gamestate_buffer := GameStateBuffer2,
               intent_buffer := [] % emptied out after applying intents
    }.

trim_gamestate_buffer(BufDepth, GameStates) ->
    Now = erlang:system_time(),
    %io:format("Gamestate is ~p~n", [State]),
    %io:format("Gamestate buffer is ~p~n", [BufferMap0]),
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
apply_entity_intents([], GameState) ->
    GameState;
apply_entity_intents([ Intent | Rest ], GameStates) ->
    Type = maps:get(type, Intent),
    ID = maps:get(id, Intent),
    [ GameState0 | _ ] = GameStates,
    GameState1 = 
        case Type of 
            'MOVE' -> 
                Phys = maps:get(phys, Intent), 
                apply_phys_intent(ID, Phys, GameState0);
            'ATTACK' ->
                % do nothing for the moment. TODO
                ok,
                GameState0
        end,
    apply_entity_intents(Rest, GameState1).


apply_phys_intent(ID, Phys, GameState) ->
    % This function will accept any new input received from the player session
    % and update the server appropriately.  We will start by applying new
    % Velocity vectors received from the client, but what we really ought to do
    % is accept only input commands and use those to calculate new
    % velocity/rotation for the entity. TODO.
    {_Pos, Vel, Rot} = ow_vector:phys_to_tuple(Phys),
    EntityInfo = entity_by_id(ID, GameState),
    % Zero out any tiny velocities to hopefully help with floating point
    % rounding issues.
    NewVel = 
        case overworld_vector:length_squared(Vel) < 0.01 of
           false ->
               Vel;
           true -> 
               {0, 0}
        end,
    % Check to see if the player is moving an entity that doesn't exist
    % Otherwise continue to update the phys
    Entity2 = EntityInfo#entity{
        vel = NewVel,
        rot = Rot
    },
    update_entity(Entity2, GameState).
    
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
        % fix me
        {Xv, Yv} = E#entity.vel,
        {Xp, Yp} = E#entity.pos,
        TickMs = TickRate / 1000,
        %Latency = get_latency(E),

        % TODO: Re-add latency
        Latency = 0,
        NewPos = {
            Xp + (Xv * (TickMs + Latency)),
            Yp + (Yv * (TickMs + Latency))
        },
        E#entity{pos = NewPos}
    end,
    NewEnts = [PositionFun(X) || X <- Entities],
    %case NewEnts of
    %    [] -> ok;
    %    _ ->
    %        io:format("New positions: ~p~n", [NewEnts])
    %end,
    GS#gamestate{entities = NewEnts}.


new_entity(Handle, ID, GameState) ->
    Entity = #{
        id => ID,
        name => Handle,
        phys => 
            #{ 
                pos => {0, 0},
                vel => {0, 0},
                rot => 0
            },
        hitbox => ow_vector:rect_to_map([
                {-20, -20},
                {-20, 20},
                {20, -20},
                {20, 20}
                 ]),
        stats => 
            #{
                max_hp => 100,
                cur_hp => 100
            }
    },
    {Entity, update_entity(Entity, GameState)}.


entity_by_id(ID, GameState) ->
    EList = GameState#gamestate.entities,
    lists:keyfind(ID, #entity.id, EList).


update_entity(Entity, GameState) ->
    EList = GameState#gamestate.entities,
    ID = Entity#entity.id,
    lists:keystore(ID, #entity.id, EList, Entity).
