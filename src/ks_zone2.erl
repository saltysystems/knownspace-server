-module(ks_zone2).
-behaviour(ow_zone).

-export([
    init/1,
    handle_join/3,
    handle_part/2,
    handle_rpc/4,
    handle_tick/2
]).

-export([
    start/0,
    stop/0,
    join/2,
    part/1,
    actor_request/2,
    input/2,
    get_env/2,
    target/2,
    admin_reconfig/2
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
-define(DEFAULT_MAX_VELOCITY, 300.0).
% Set the default max visibility range, in px
-define(DEFAULT_MAX_VISIBILITY, 10000).
% Set the default max rotational velocity (in radians/sec)
-define(DEFAULT_MAX_ROTATION, 2.00).
% Set the default tick rate in ms per tick
-define(DEFAULT_TICK_RATE, 100).
% Scaling factors for rotational acceleration and acceleration behaviour
-define(DEFAULT_RACC_FACTOR, 1.0).
-define(DEFAULT_ACC_FACTOR, 1.0).
% The maximum number of frames that a client can be behind (or ahead)
-define(DEFAULT_FRAME_WINDOW, 5).

%% API

-rpc_client([
              join,
              part,
              zone_transfer,
              {zone_snapshot, #{ qos => reliable, channel => 1 }},
              actor,
              target_info,
              admin_reconfig
             ]).
-rpc_server([
              join,
              part,
              input,
              actor_request,
              target,
              admin_reconfig
             ]).

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

% TODO: Harden all public functions against bad inputs
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

actor_request(Msg, Session) ->
    ow_zone:rpc(?SERVER, actor_request, Msg, Session).

target(Msg, Session) ->
    ow_zone:rpc(?SERVER, target, Msg, Session).

admin_reconfig(Msg, Session) ->
    ow_zone:rpc(?SERVER, admin_reconfig, Msg, Session).

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
    {ok, W1} = ow_ecs2:add_system({ks_phys2, proc_phys, 2}, 200, World),
    {ok, W2} = ow_ecs2:add_system({ks_reactor, proc_reactor, 2}, 900, W1),
    %{ok, W3} = ow_ecs2:add_system({ks_projectile, proc_projectile, 1}, 100, W2),
    {ok, W4} = ow_ecs2:add_system({ks_collision, proc_collision, 2}, 300, W2),
    %{ok, W5} = ow_ecs2:add_system({ks_visibility, proc_target_info, 2}, 800, W4),
    % Configure the initial state
    TickMs = ?DEFAULT_TICK_RATE,
    Config = #{tick_ms => TickMs},
    {ok, BoundaryPid} = ks_quadtree:start_link(?DEFAULT_BOUNDARY, 5),
    Window = ?DEFAULT_FRAME_WINDOW,
    InitialZoneState =
        #{
            % milliseconds
            buffer_depth => ?DEFAULT_BUFFER_DEPTH,
            % radial
            boundary => BoundaryPid,
            ecs_world => W4,
            % not ideal this has to be specified twice
            tick_ms => TickMs,
            env => init_environment(),
            % Frame number
            frame => 0,
            window => Window
        },
    logger:notice("Zone ~p:~p starting up. World ID: ~p", [?SERVER, self(), World]),
    {ok, InitialZoneState, Config}.

handle_join(Msg, Session, State = #{ecs_world := World}) ->
    ID = ow_session:get_id(Session),
    Handle = maps:get(handle, Msg),
    logger:notice("Player ~p:~p has joined the server!", [Handle, ID]),
    % Add the handle to the player info
    PlayerInfo = #{handle => Handle},
    % Add the actor to the ECS
    ShipType = maps:get(ship, Msg, "normal"),
    Actor = ks_actor:new(Handle, ShipType, ID, World),
    % Encode in network format
    ActorNetFmt = ow_netfmt:to_proto(Actor),
    % Let everyone else know that the Player has joined
    ow_zone:broadcast(self(), {actor, ActorNetFmt}),
    %% Create an input buffer for this player, linked to the zone
    %ks_input:start(Frame, FW, ID),
    % Build a reply to the player with information about actors who joined
    % before them.
    % TODO: Replace with nearby entities and let the player request the ones
    % they want
    Actors = lists:delete(ID, ks_actor:ids(World)),
    % frame, tick_ms and env are already stored in the State. So just add
    % entities and projectiles.
    ZoneXfer = State#{
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
            %% Kill the player's input buffer
            %ks_input:stop(ID),
            % Let everyone know that the Player departed
            Msg = #{id => ID},
            Reply = {'@zone', {part, Msg}},
            {Reply, ok, State}
    end.

handle_rpc(input, Msg, Session, State) ->
    % Push an input sequence to the player's buffer
    ID = ow_session:get_id(Session),
    ks_input_comp:push(Msg, ID),
    {noreply, ok, State};
handle_rpc(actor_request, Msg, Session, State = #{ecs_world := World}) ->
    % Respond with info about the actor
    ID = ow_session:get_id(Session),
    % Actor may not exist by the time we get the request, but try to lookup and
    % respond
    EntityID = maps:get(id, Msg),
    case ks_actor:get_net(EntityID, World) of
        false ->
            {noreply, ok, State};
        Actor ->
            Reply = {{'@', [ID]}, {actor, ow_netfmt:to_proto(Actor)}},
            {Reply, ok, State}
    end;
handle_rpc(area_search, Msg, Session, State) ->
    % Internal RPCs - don't expose these to external clients without
    % considering the ramifications!
    % Addendum: Maybe this is a natural way for clients to reduce the amount of
    % neighbours they're quering for? I don't remember what the negative
    % ramifications were.
    % --
    % Get the boundary and ECS handle
    #{ecs_world := World} = State,
    % Should we really transmit the session
    ID = ow_session:get_id(Session),
    % IDs around or make new ones?? See NOTES-1
    #{range := Range} = Msg,
    Entity = ow_ecs2:entity(ID, World),
    #{pos_t := Pos} = ow_ecs2:get(kinematics, Entity),
    QuadTree = ks_quadtree:get_tree(),
    Results = ks_area:search(Pos, Range, QuadTree, World),
    Reply = {{'@', [ID]}, {area_result, Results}},
    {Reply, ok, State};
handle_rpc(target, Msg, Session, #{env := Env, ecs_world := World} = State) ->
    PlayerID = ow_session:get_id(Session),
    % TODO : Harden input handling
    #{id := TargetID} = Msg,
    Results = ks_visibility:target_info(PlayerID, TargetID, Env, World),
    Reply = {{'@', [PlayerID]}, {target_info, ow_netfmt:to_proto(Results)}}, 
    {Reply, ok, State};
handle_rpc(admin_reconfig, Msg, _Session, #{env := ZoneEnv} = State) ->
    ReconfigEnv = maps:get(env, Msg, #{}),
    NewEnv = maps:merge(ZoneEnv, ReconfigEnv),
    Msg1 = #{
        env => NewEnv
    },
    Reply = {'@zone', {admin_reconfig, Msg1}},
    {Reply, ok, State#{env => NewEnv}}.

handle_tick(TickMs, #{ ecs_world := World, frame := Frame, window := Window} = State) ->
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
    %% Slide all input windows forward 1 frame
    %Players = ow_player_reg:list(self()),
    %[ ks_input:slide(Window,ow_player_reg:get_id(P)) || P <- Players ],
    % Create the zone snapshot
    ToXfer = #{
        actors => ks_actor:updates_net(World)
        %projectiles => ks_projectile:notify(World),
        %collisions => ks_collision:notify(World)
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
        max_vel_r => ?DEFAULT_MAX_ROTATION,
        max_visibility => ?DEFAULT_MAX_VISIBILITY,
        acc_factor => ?DEFAULT_ACC_FACTOR,
        racc_factor => ?DEFAULT_RACC_FACTOR
    }.
