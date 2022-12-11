-module(ks_zone2).
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
-define(KS_ZONE_ACTOR, 16#2010).

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
            opcode => ?KS_ZONE_ACTOR,
            s2c_call => actor,
            encoder => ks_pb,
            qos => reliable,
            channel => 0
        }
    ].

%%%====================================================================
%%% Structures and Types
%%%====================================================================
%-type zone_boundary() :: number() | {number(), number(), number(), number()}.

-record(gamestate, {
    actors = [] :: list(),
    projectiles = [] :: list(),
    new_projectiles = [] :: list(),
    collisions = [] :: list(),
    timestamp :: integer()
}).
%-type gamestate() :: #gamestate{}.

%-type id() :: integer().

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
    World = zone,
    TickMs = 50,
    InitialZoneState =
        #{
            input_buffer => [],
            gamestate_buffer => [GameState],
            % milliseconds
            buffer_depth => ?DEFAULT_BUFFER_DEPTH,
            % radial
            boundary => ?DEFAULT_BOUNDARY,
            ecs_world => World,
            tick_ms => TickMs
        },
    Config = #{tick_ms => TickMs},
    % Start the ECS server. This should be handled by a supervisor
    ow_ecs:start_link(World),
    % Get the query obj
    Query = ow_ecs:query(World),
    % Add the systems
    ow_ecs:add_system({ks_phys, proc_phys, 2}, 200, Query),
    ow_ecs:add_system({ks_reactor, proc_reactor, 2}, 900, Query),
    ow_ecs:add_system({ks_projectile, proc_projectile, 1}, 100, Query),
    ow_ecs:add_system({ks_collision, proc_collision, 2}, 300, Query),
    ow_ecs:add_system({ks_input, proc_reset, 1}, 900, Query),
    {ok, InitialZoneState, Config}.

handle_join(Msg, Session, State = #{ecs_world := World}) ->
    ID = ow_session:get_id(Session),
    Handle = maps:get(handle, Msg),
    logger:notice("Player ~p:~p has joined the server!", [Handle, ID]),
    % Add the handle to the player info
    PlayerInfo = #{handle => Handle},
    % Add the actor to the ECS
    Actor = ks_actor:new(Handle, ID, World),
    % Let everyone else know that the Player has joined
    ow_zone:broadcast(self(), {actor, ks_actor:map(Actor)}),
    % Build a reply to the player with information about actors who joined
    % before them.
    ZoneXfer = #{
        tick_ms => maps:get(tick_ms, State),
        actors => ks_actor:get_all(World),
        projectiles => ks_projectile:notify(World)
    },
    Reply = {{'@', [ID]}, {zone_transfer, ZoneXfer}},
    % Reply to the player, update the player registry and the zone state
    {Reply, {ok, Session, PlayerInfo}, State}.

handle_part(Session, State = #{ecs_world := World}) ->
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
            % Remove the player from the actors list
            ks_actor:rm(ID, World),
            % Let everyone know that the Player departed
            Msg = #{id => ID},
            Reply = {'@zone', {part, Msg}},
            {Reply, ok, State}
    end.

handle_rpc(input, Msg, Session, State = #{ecs_world := World}) ->
    % Inject the ID of the player moving into the Msg
    ID = ow_session:get_id(Session),
    %Latency = ow_session:get_latency(Session),
    % Update the latency
    %ks_actor:update_latency(Latency, ID, World),
    ks_input:push(Msg, ID, World),
    {noreply, ok, State}.

handle_tick(TickMs, State = #{ecs_world := World}) ->
    %State1 = update_gamestate(TickMs, State),
    %Snapshot = gamestate_snapshot(State), % TODO
    %#{gamestate_buffer := [ Snapshot | GSBuf ]} = State1,
    % Call systems, feed in relevant zone data if necessary
    ZoneData = maps:with([boundary], State),
    ow_ecs:proc(World, ZoneData#{tick_ms => TickMs}),
    ToXfer = #{
        phys_updates => get_actor_phys(World),
        projectiles => ks_projectile:notify(World),
        collisions => ks_collision:notify(World)
    },
    Reply = {'@zone', {zone_snapshot, ToXfer}},
    {Reply, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

-spec get_actor_phys(term()) -> [map()].
get_actor_phys(World) ->
    % Get all actors
    ActorMap = ks_actor:get_all(World, map),
    [maps:with([id, phys], Actor) || Actor <- ActorMap].
