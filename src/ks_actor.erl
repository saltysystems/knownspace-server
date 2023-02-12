-module(ks_actor).

-export([new/3, map/1, rm/2, get_all/1, update_latency/3]).

-type id() :: ow_session:id().
-type actor() :: {id(), [term()]}.
-export_type([actor/0]).

-spec new(string(), integer(), atom()) -> map().
new(Handle, ID, World) ->
    % Create a new ship 
    Coords = {rand:uniform(256), rand:uniform(256)},
    ks_ship:new(Coords, hauler, ID, Handle, World),
    % Get the ship in network format
    Ship = ks_shipgrid:netformat(ID, World),
    % Add the handle 
    #{
      id => ID,
      handle => Handle,
      ship => Ship
     }.

-spec rm(id(), atom()) -> ok.
rm(ID, World) ->
    ow_ecs2:rm_entity(ID, World).

get_all(World) ->
    Actors = ow_ecs2:match_component(actor, World),
    F = 
        fun({ID, Components}, AccIn) ->
            A = #{ id => ID,
                   handle => ow_ecs2:get(handle, Components, unknown),
                   ship => ks_shipgrid:netformat(ID, World)
                 },
            [A | AccIn]
        end,
    lists:foldl(F, [], Actors).

-spec map(ow_ecs2:entity()) -> map().
map(Actor) ->
    ow_ecs2:to_map(Actor).

-spec update_latency(pos_integer(), id(), atom()) -> ok.
update_latency(Latency, ID, World) ->
    ow_ecs2:add_component(latency, Latency, ID, World).
