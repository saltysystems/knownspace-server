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
    input/2,
    get_env/2
]).

% NPC functions
-export([
    area_search/2
]).

% Allow instantiating multiple instances of zones
%-define(SERVER(Name), {via, gproc, {n, l, {?MODULE, Name}}}).
-define(SERVER, ?MODULE).
% Radial boundary size
-define(DEFAULT_BOUNDARY, 1000).
% Set the default buffer depth in milliseconds
-define(DEFAULT_BUFFER_DEPTH, 500).
% Set the default max velocity in this zone
-define(DEFAULT_MAX_VELOCITY, 300).
% Set the default max rotational velocity (in radians/sec)
-define(DEFAULT_MAX_ROTATION, math:pi()/2).
% Set the default tick rate in ms per tick
-define(DEFAULT_TICK_RATE, 50).

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
            qos => reliable,
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
% none

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

% Privileged RPCs (server-side)
area_search(Msg, Session) ->
    ow_zone:rpc(?SERVER, area_search, Msg, Session).

-spec get_env(atom(), map()) -> term().
get_env(Key, #{env := Env}) ->
    maps:get(Key, Env, undefined).

%%%====================================================================
%%% Callbacks
%%%====================================================================
init([]) ->
    % Initialize the zone with empty buffers and some default parameters
    % Add the systems
    World = ow_ecs2:start(),
    {ok, W1} = ow_ecs2:add_system({ks_phys, proc_phys, 2}, 200, World),
    {ok, W2} = ow_ecs2:add_system({ks_reactor, proc_reactor, 2}, 900, W1),
    {ok, W3} = ow_ecs2:add_system({ks_projectile, proc_projectile, 1}, 100, W2),
    {ok, W4} = ow_ecs2:add_system({ks_collision, proc_collision, 2}, 300, W3),
    %%ow_ecs:add_system({ks_collision_ray, proc_collision, 2}, 300, World),
    {ok, W6} = ow_ecs2:add_system({ks_input, proc_reset, 1}, 900, W4),
    %{ok, W6} = ow_ecs2:add_system({ks_input, proc_debug, 2}, 900, W5),
    % Configure the initial state
    TickMs = ?DEFAULT_TICK_RATE, 
    Config = #{tick_ms => TickMs},
    InitialZoneState =
        #{
            input_buffer => [],
            % milliseconds
            buffer_depth => ?DEFAULT_BUFFER_DEPTH,
            % radial
            boundary => ow_collision:new(?DEFAULT_BOUNDARY, 5),
            ecs_world => W6,
            % not ideal this has to be specified twice
            tick_ms => TickMs,
            env => init_environment(),
            frame => 0 % Frame number
        },
    {ok, InitialZoneState, Config}.

handle_join(Msg, Session, State = #{ecs_world := World}) ->
    ID = ow_session:get_id(Session),
    Handle = maps:get(handle, Msg),
    logger:notice("Player ~p:~p has joined the server!", [Handle, ID]),
    % Add the handle to the player info
    PlayerInfo = #{handle => Handle},
    % Add the actor to the ECS
    Actor = ks_actor:new(Handle, ID, World),
    % Encode in network format
    ActorNetFmt = ow_netfmt:to_proto(Actor),
    % Let everyone else know that the Player has joined
    ow_zone:broadcast(self(), {actor, ActorNetFmt}),
    % Build a reply to the player with information about actors who joined
    % before them.
    Actors = ks_actor:get_all_net(World),
    ZoneXfer = #{
        tick_ms => maps:get(tick_ms, State),
        env => maps:get(env, State),
        entities => Actors,
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
    {noreply, ok, State};
handle_rpc(area_search, Msg, Session, State) ->
    % Internal RPCs - don't expose these to external clients without
    % considering the ramifications!
    % Get the boundary and ECS handle
    #{ecs_world := World, boundary := QuadTree} = State,
    % Should we really transmit the session
    ID = ow_session:get_id(Session),
    % IDs around or make new ones?? See NOTES-1
    #{range := Range} = Msg,
    Entity = ow_ecs2:entity(ID, World),
    #{pos_t := Pos} = ow_ecs2:get(kinematics, Entity),
    Results = ks_area:search(Pos, Range, QuadTree, World),
    Reply = {{'@', [ID]}, {area_result, Results}},
    {Reply, ok, State}.

handle_tick(TickMs, State = #{ecs_world := World, frame := Frame}) ->
    % Call systems, feed in relevant zone data if necessary
    Start = erlang:system_time(),
    ZoneData = State,
    ow_ecs2:proc(ZoneData, World),
    End = erlang:system_time(),
    Delta = erlang:convert_time_unit(End - Start, native, millisecond),
    case Delta > TickMs / 2 of
        true ->
            logger:notice("Frame delta more than half of frame time!");
        false ->
            ok
    end,
    %io:format("Phys updates: ~p~n", [get_actor_phys(World)]),
    ToXfer = #{
        actors => ks_actor:updates_net(World),
        projectiles => ks_projectile:notify(World),
        collisions => ks_collision:notify(World)
    },
    Reply = {'@zone', {zone_snapshot, ToXfer}},
    {Reply, State#{frame := Frame + 1}}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

-spec init_environment() -> map().
init_environment() ->
    % Initialize the Environment.
    #{
        max_vel_t => ?DEFAULT_MAX_VELOCITY,
        max_vel_r => ?DEFAULT_MAX_ROTATION
    }.
