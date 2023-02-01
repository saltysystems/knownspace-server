-module(ks_ship).

-export([new/5]).
-export([simple/2]).
-export([hauler/2]).

new(Coordinates, Type, ID, Handle, World) ->
    ks_shipgrid:new(ID, World),
    erlang:apply(ks_ship, Type, [ID, World]),
    % Now add the other required pieces
    ow_ecs:add_component(handle, Handle, ID, World),
    ow_ecs:add_component(actor, true, ID, World),
    Phys =
        #{
            pos => ow_vector:vector_map(Coordinates),
            vel => ow_vector:vector_map({0, 0}),
            rot => 0
        },
    ow_ecs:add_component(phys, Phys, ID, World),
    % Return the final grid
    ow_ecs:entity(ID, World).

simple(ID, World) ->
    ks_shipgrid:add({0, 0}, omni, 0, ID, World).

hauler(ID, World) ->
    % Rightside
    ks_shipgrid:add({0,0}, gun, 2, ID, World),
    ks_shipgrid:add({0,1}, cargo, 3, ID, World),
    ks_shipgrid:add({0,2}, cargo, 3, ID, World),
    ks_shipgrid:add({0,3}, rtg, 0, ID, World),
    ks_shipgrid:add({0,4}, rocket, 0, ID, World),
    % Leftside
    ks_shipgrid:add({-1,0}, beam, 0, ID, World),
    ks_shipgrid:add({-1,1}, cargo, 1, ID, World),
    ks_shipgrid:add({-1,2}, cargo, 1, ID, World),
    ks_shipgrid:add({-1,3}, gyroscope, 0, ID, World),
    ks_shipgrid:add({-1,4}, rocket, 0, ID, World).
