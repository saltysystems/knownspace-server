-module(ks_ship).

-export([simple/2]).
-export([normal/2]).
-export([hauler/2]).

simple(ID, World) ->
    ks_shipgrid:add({0, 0}, omni, 0, ID, World).

normal(ID, World) ->
    ks_shipgrid:add({0, 1}, beam, 0, ID, World),
    ks_shipgrid:add({2, 1}, beam, 0, ID, World),
    ks_shipgrid:add({0, 2}, gyroscope, 0, ID, World),
    ks_shipgrid:add({1, 2}, rtg, 0, ID, World),
    ks_shipgrid:add({2, 2}, gyroscope, 0, ID, World),
    ks_shipgrid:add({0, 3}, rocket, 0, ID, World),
    ks_shipgrid:add({2, 3}, rocket, 0, ID, World).

hauler(ID, World) ->
    % Rightside
    ks_shipgrid:add({0, 0}, gun, 2, ID, World),
    ks_shipgrid:add({0, 1}, cargo, 3, ID, World),
    ks_shipgrid:add({0, 2}, gyroscope, 3, ID, World),
    ks_shipgrid:add({0, 3}, rtg, 0, ID, World),
    ks_shipgrid:add({0, 4}, rocket, 0, ID, World),
    % Leftside
    ks_shipgrid:add({-1, 0}, beam, 0, ID, World),
    ks_shipgrid:add({-1, 1}, cargo, 1, ID, World),
    ks_shipgrid:add({-1, 2}, cargo, 1, ID, World),
    ks_shipgrid:add({-1, 3}, gyroscope, 0, ID, World),
    ks_shipgrid:add({-1, 4}, rocket, 0, ID, World).
