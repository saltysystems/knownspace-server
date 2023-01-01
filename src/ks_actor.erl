-module(ks_actor).

-export([new/3, map/1, rm/2, get_all/2, get_all/1, update_latency/3]).

-type id() :: ow_session:id().
-type actor() :: {id(), [term()]}.
-export_type([actor/0]).

-spec new(string(), id(), atom()) -> actor().
new(Handle, ID, World) ->
    % TBD
    Type = 0,
    Phys =
        #{
            pos => ow_vector:vector_map({rand:uniform(1024), rand:uniform(1024)}),
            vel => ow_vector:vector_map({0, 0}),
            rot => 0
        },
    Hitbox = ow_vector:rect_to_maps([
        {-20, -20},
        {-20, 20},
        {20, -20},
        {20, 20}
    ]),
    Stats =
        #{
            max_hp => 100,
            cur_hp => 100,
            % rotation / rotation_factor
            rotation_fac => 10,
            speed_fac => 5,
            max_vel => 400
        },
    Latency = 0,
    Reactor = #{
        cur_reactor => 0,
        max_reactor => 300,
        rate_reactor => 5
    },
    Query = ow_ecs:query(World),
    ow_ecs:add_component(actor, true, ID, Query),
    ow_ecs:add_component(handle, Handle, ID, Query),
    ow_ecs:add_component(type, Type, ID, Query),
    ow_ecs:add_component(phys, Phys, ID, Query),
    ow_ecs:add_component(hitbox, Hitbox, ID, Query),
    ow_ecs:add_component(stats, Stats, ID, Query),
    ow_ecs:add_component(latency, Latency, ID, Query),
    ow_ecs:add_component(reactor, Reactor, ID, Query),
    % Always get the actor back from the ETS table
    ow_ecs:entity(ID, Query).

-spec map(ow_ecs:entity()) -> map().
map(Actor) ->
    ow_ecs:to_map(Actor).

-spec rm(id(), atom()) -> ok.
rm(ID, World) ->
    Query = ow_ecs:query(World),
    ow_ecs:rm_entity(ID, Query).

-spec update_latency(pos_integer(), id(), atom()) -> ok.
update_latency(Latency, ID, World) ->
    Query = ow_ecs:query(World),
    ow_ecs:add_component(latency, Latency, ID, Query).

-spec get_all(atom(), term()) -> [map()].
get_all(World, map) ->
    [map(Actor) || Actor <- get_all(World)].

-spec get_all(atom()) -> [map()].
get_all(World) ->
    Query = ow_ecs:query(World),
    ow_ecs:match_component(actor, Query).
