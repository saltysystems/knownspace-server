-module(ks_ship).

-export([box/2]).
-export([normal/2]).
-export([broadside/2]).
-export([ywing/2]).
-export([hauler/2]).

% broken
box(ID, World) ->
    ks_shipgrid:add({0, 0}, rtg, 0, ID, World),
    % guns
    ks_shipgrid:add({1, -1}, beam, 0, ID, World),
    ks_shipgrid:add({-1, -1}, beam, 0, ID, World),
    % gyros
    ks_shipgrid:add({1, 1}, gyroscope, 0, ID, World),
    ks_shipgrid:add({-1, 1}, gyroscope, 0, ID, World),
    % thrusters 
    ks_shipgrid:add({0, 1}, rocket, 0, ID, World),
    ks_shipgrid:add({0, -1}, rocket, 2, ID, World).

normal(ID, World) ->
    ks_shipgrid:add({0, 1}, beam, 0, ID, World),
    ks_shipgrid:add({2, 1}, beam, 0, ID, World),
    ks_shipgrid:add({0, 2}, gyroscope, 0, ID, World),
    ks_shipgrid:add({1, 2}, rtg, 0, ID, World),
    ks_shipgrid:add({2, 2}, gyroscope, 0, ID, World),
    ks_shipgrid:add({0, 3}, rocket, 0, ID, World),
    ks_shipgrid:add({2, 3}, rocket, 0, ID, World).

broadside(ID, World) ->
    % Leftside
    ks_shipgrid:add({-1,1}, beam, 1, ID, World),
    ks_shipgrid:add({-1,2}, beam, 1, ID, World),
    ks_shipgrid:add({-1,3}, beam, 1, ID, World),
    ks_shipgrid:add({-1,4}, beam, 1, ID, World),
    ks_shipgrid:add({-1,5}, rocket, 0, ID, World),
    % Middle
    ks_shipgrid:add({0,1}, rocket, 3, ID, World),
    ks_shipgrid:add({0,2}, rtg, 0, ID, World),
    ks_shipgrid:add({0,3}, gyroscope, 0, ID, World),
    ks_shipgrid:add({0,4}, gyroscope, 0, ID, World),
    ks_shipgrid:add({0,5}, cargo, 0, ID, World),
    % Rightside
    ks_shipgrid:add({1,1}, beam, 3, ID, World),
    ks_shipgrid:add({1,2}, beam, 3, ID, World),
    ks_shipgrid:add({1,3}, beam, 3, ID, World),
    ks_shipgrid:add({1,4}, beam, 3, ID, World),
    ks_shipgrid:add({1,5}, rocket, 0, ID, World).

%ywing(ID, World) ->
%    % Center
%    ks_shipgrid:add({0,0}, beam, 0, ID, World),
%    ks_shipgrid:add({0,1}, rtg, 0, ID, World),
%    ks_shipgrid:add({0,2}, gyroscope, 0, ID, World),
%    ks_shipgrid:add({0,3}, gyroscope, 0, ID, World),
%    ks_shipgrid:add({0,4}, cargo, 0, ID, World),
%    % Left
%    ks_shipgrid:add({-1,3}, beam, 0, ID, World),
%    ks_shipgrid:add({-1,4}, cargo, 1, ID, World),
%    ks_shipgrid:add({-1,5}, cargo, 1, ID, World),
%    ks_shipgrid:add({-1,6}, rocket, 0, ID, World),
%    % Right
%    ks_shipgrid:add({1,3}, beam, 0, ID, World),
%    ks_shipgrid:add({1,4}, cargo, 3, ID, World),
%    ks_shipgrid:add({1,5}, cargo, 3, ID, World),
%    ks_shipgrid:add({1,6}, rocket, 0, ID, World).

ywing(ID, World) ->
    % Rightside
    ks_shipgrid:add({0, 0}, beam, 0, ID, World),
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


hauler(ID, World) ->
    % Rightside
    ks_shipgrid:add({0, 0}, beam, 0, ID, World),
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
