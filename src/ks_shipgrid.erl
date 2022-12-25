-module(ks_shipgrid).

% @doc 
% The ShipGrid module provides an interface for calculating various Group
% components as used by the actor module
% @end 

-export([new/2, add/4, match_subcomponents/3]).

-include("modules.hrl").
-define(MAX_GRID, 5).

%%-------------------------------------------------------------------
%% Public API
%%-------------------------------------------------------------------

new(ID,World) ->
    G = ow_grid2d:new(?MAX_GRID, ?MAX_GRID),
    Q = ow_ecs:query(World),
    ow_ecs:add_component(shipgrid, G, ID, Q).

add(Coordinate, ModuleType, ParentID, World) ->
    Query = ow_ecs:query(World),
    case ow_ecs:try_component(shipgrid, ParentID, Query) of
        false ->
            {error, no_grid};
        {Grid, Rest} -> 
            Children = ow_ecs:get(children, Rest, []),
            ChildRef = instance_module(ModuleType, 0, ParentID, Query),
            Children2 = [ ChildRef | Children ],
            Grid2 = ow_grid2d:put(Coordinate, ChildRef, Grid),
            % Update the grid
            Components = [
                          {shipgrid, Grid2},
                          {children, Children2}
                         ],
            ow_ecs:add_components(Components, ParentID, Query)
    end.

match_subcomponents(Component, ParentID, World) ->
    Query = ow_ecs:query(World),
    case ow_ecs:try_component(children, ParentID, Query) of
        false -> []; % no children, so the match is always emptylist
        {Children, _Rest} -> 
            F = 
                fun(ChildID) ->
                    case ow_ecs:try_component(Component, ChildID, Query) of
                        false -> 
                            false;
                        {Data, _DataRest} -> 
                            {true, Data}
                    end
                end,
            lists:filtermap(F, Children)
    end.

% WIP..
%pivot(ID, World) -> 
%    % Calculate the pivot point by first calculating the pivot point of each
%    % component, then summing the sums.
%    % 
%    % Assume each cell to be 10px by 10 px, such that a rectangle spanning the
%    % area is defined as [{-5,-5},{5,-5},{5,5},{-5,5}]
%    Hitboxes = match_subcomponents(hitbox, ID, World),
%    % Unzip the vectors
%    F = fun(Vectors) ->
%            {XList, YList} = lists:unzip(Vectors),
%            {lists:sum(XList), lists:sum(YList)}
%        end,
%    lists:foldl(
    

%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------

instance_module(Type, Orientation, ParentID, Query) ->
    % Get the default configuration from file
    DefaultModules = ?DEFAULT_MODULES,
    % Components added at instance time
    Components = [
                  {orientation, Orientation},
                  {parent, ParentID}
                 ],
    % Get the relevant module from the include
    DefaultComponents = maps:get(Type, DefaultModules),
    % Create the component instance in ECS
    ModuleID = erlang:unique_integer(),
    ow_ecs:add_components(Components ++ DefaultComponents, ModuleID, Query),
    % Return the child ID back to the caller
    ModuleID.
