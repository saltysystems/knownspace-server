-module(ks_ship).

-export([new/4]).

new(Coordinates, Type, ID, World) ->
    Query = ow_ecs:query(World),
    ks_shipgrid:new(ID, World),
    case Type of
        simple ->
            simple(ID, World);
        thrusta ->
            thrusta(ID, World);
        bigrig ->
            bigrig(ID, World)
    end,
    % Now add the other required pieces
    ow_ecs:add_component(actor, true, ID, Query),
    Phys =
        #{
            pos => ow_vector:vector_map(Coordinates),
            vel => ow_vector:vector_map({0, 0}),
            rot => 0
        },
    ow_ecs:add_component(phys, Phys, ID, Query),
    % Return the final grid
    ow_ecs:entity(ID, Query).

simple(ID, World) ->
    ks_shipgrid:add({0, 0}, omni, 0, ID, World).

thrusta(ID, World) ->
    ks_shipgrid:add({0, 0}, omni, 0, ID, World),
    ks_shipgrid:add({0, -1}, rocket, 0, ID, World).

bigrig(ID, World) ->
    ks_shipgrid:add({0, 0}, rtg, 0, ID, World),
    ks_shipgrid:add({1, 0}, beam, 0, ID, World),
    ks_shipgrid:add({-1, 0}, beam, 0, ID, World),
    ks_shipgrid:add({1, -1}, rocket, 0, ID, World),
    ks_shipgrid:add({-1, -1}, rocket, 0, ID, World),
    ks_shipgrid:add({0, -1}, rocket, 0, ID, World).
