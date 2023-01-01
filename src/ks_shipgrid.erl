-module(ks_shipgrid).

% @doc
% The ShipGrid module provides an interface for calculating various Group
% components as used by the actor module
% @end

-export([new/2, del/2, add/5, match_subcomponents/3, map/1]).

-include("modules.hrl").
% px
-define(CELL_SIZE, 10).

-opaque shipgrid() :: {map(), integer(), atom()}.
-export_type([shipgrid/0]).

-type vector() :: ow_vector:vector().
-type rotation() :: 0 | 1 | 2 | 3.
-type id() :: integer().

%%-------------------------------------------------------------------
%% Public API
%%-------------------------------------------------------------------

-spec new(integer(), atom()) -> ok.
new(ID, World) ->
    Grid = ow_sparsegrid:new(),
    Q = ow_ecs:query(World),
    % Clean up any old components
    del(ID, World),
    % Add the new, clean component
    ow_ecs:add_component(shipgrid, Grid, ID, Q).

-spec del(integer(), atom()) -> ok.
del(ID, World) ->
    Q = ow_ecs:query(World),
    case ow_ecs:try_component(shipgrid, ID, Q) of
        false ->
            ok;
        Components ->
            % Get the children and clean them up.
            Children = ow_ecs:get(children, Components),
            case Children of
                false ->
                    ok;
                _ ->
                    F = fun(ChildID) ->
                        ow_ecs:rm_entity(ChildID, Q)
                    end,
                    lists:foreach(F, Children),
                    ow_ecs:del_component(children, ID, Q)
            end,
            % Remove the shipgrid
            ow_ecs:del_component(shipgrid, ID, Q)
    end.

-spec add(vector(), atom(), rotation(), id(), atom()) -> ok.
add(Coords, Type, Rotation, ParentID, World) ->
    Query = ow_ecs:query(World),
    case ow_ecs:try_component(shipgrid, ParentID, Query) of
        false ->
            {error, no_grid};
        Data ->
            Grid = ow_ecs:get(shipgrid, Data),
            Children = ow_ecs:get(children, Data, []),
            % Instance the new child module and update the grid
            ChildRef = instance_module(Coords, Type, Rotation, ParentID, Query),
            Children2 = [ChildRef | Children],
            Grid2 = ow_sparsegrid:put(Coords, ChildRef, Grid),
            % Update the entity
            Components = [
                {shipgrid, Grid2},
                {children, Children2}
            ],
            ow_ecs:add_components(Components, ParentID, Query),
            % Recalculate pivot point, thrust, etc. This may need to be a fun
            % with callbacks as it grows. Can't predict what properties need to
            % be present at the macro level
            Pivot = pivot(ParentID, World),
            % Get the current reactor stats before updating
            CurReactor = ow_ecs:get(reactor, Data, ks_reactor:new()),
            Reactor = reactor(CurReactor, ParentID, World),
            Thrust = thrust(ParentID, World),
            Hull = hull(ParentID, World),
            % Pass another update to the entity with 2nd-order calculated items
            Components2 = [
                {pivot, Pivot},
                {reactor, Reactor},
                {engine, Thrust},
                {hull, Hull}
            ],
            ow_ecs:add_components(Components2, ParentID, Query)
    end.
% I'd like to come up with a good boundary where the side-effecty stuff is
% wrapped..
%E = ow_ecs:entity(ParentID, Query),
%FinalGrid = ow_ecs:get(shipgrid, E),
%{FinalGrid, ParentID, World}.

match_subcomponents(Component, ParentID, World) ->
    Query = ow_ecs:query(World),
    case ow_ecs:try_component(children, ParentID, Query) of
        % no children, so the match is always emptylist
        false ->
            [];
        Data ->
            Children = ow_ecs:get(children, Data),
            F =
                fun(ChildID) ->
                    case ow_ecs:try_component(Component, ChildID, Query) of
                        false ->
                            false;
                        ChildData ->
                            % Insert the child ID into the data
                            {true, [{id, ChildID} | ChildData]}
                    end
                end,
            lists:filtermap(F, Children)
    end.

pivot(ID, World) ->
    % Calculate the pivot point by first calculating the pivot point of each
    % component, then summing the sums.
    %
    % Assume each cell to be 10px by 10 px, such that a rectangle spanning the
    % area is defined as [{-5,-5},{5,-5},{5,5},{-5,5}]
    WithHitboxes = match_subcomponents(hitbox, ID, World),
    % Unzip the vectors
    F = fun(ComponentList, AccIn) ->
        % Get the hitbox and cell
        Hitbox = ow_ecs:get(hitbox, ComponentList),
        % Assume each cell to be 10px by 10 px,
        %Cell = ow_ecs:get(grid_coords, ComponentList),
        %{XScale, YScale} = ow_vector:scale(Cell, ?CELL_SIZE),
        % Sum columns and rows
        {XList, YList} = lists:unzip(Hitbox),
        ModulePivot = {lists:sum(XList), lists:sum(YList)},
        % Translate the pivot by its cell position
        Cell = ow_ecs:get(grid_coords, ComponentList),
        ScaledCell = ow_vector:scale(Cell, ?CELL_SIZE),
        [NormalizedPivot] = ow_vector:translate([ModulePivot], ScaledCell),
        % Add the normalized pivot to the running total
        ow_vector:add(NormalizedPivot, AccIn)
    end,
    Pivot = lists:foldl(F, {0, 0}, WithHitboxes),
    Normalization = length(WithHitboxes),
    ow_vector:scale(Pivot, 1 / Normalization).

hull(ID, World) ->
    % This function calculates an outer hull of a collection of 2D polygons
    % which start out semi-triangularized. By detecting shared edges and
    % deleting them, we are left with only edges that are on the outside of the
    % ship.
    % --
    % First grab all components on the ship that have a hitbox.
    WithHitboxes = match_subcomponents(hitbox, ID, World),
    % Construct a map of edges with the ID of the child component holding the
    % edge. This map will be fed into collision detection.
    F = fun(ComponentList, AccIn) ->
        % Get child ID we inject into the component list
        ChildID = ow_ecs:get(id, ComponentList),
        % Get the hitbox
        Hitbox = ow_ecs:get(hitbox, ComponentList),
        % Scale the cell by px-per-cell
        Cell = ow_ecs:get(grid_coords, ComponentList),
        ScaledCell = ow_vector:scale(Cell, ?CELL_SIZE),
        % Translate the hitbox by the scaled position
        THB = ow_vector:translate(Hitbox, ScaledCell),
        % Calculate edges
        Edges = ow_vector:edges(THB),
        G = fun(Edge, Acc) ->
            % Sort edges as vertices can appear in any order.
            % TODO: Need to consider whether the vertex order needs to
            %       be preserved in the final output
            SortEdge = lists:sort(Edge),
            % If a duplicate edge is detected, delete it as it must be
            % a shared edge
            case maps:is_key(SortEdge, Acc) of
                true ->
                    maps:remove(SortEdge, Acc);
                false ->
                    Acc#{SortEdge => ChildID}
            end
        end,
        lists:foldl(G, AccIn, Edges)
    end,
    lists:foldl(F, #{}, WithHitboxes).

reactor(#{cur_reactor := Cur}, ID, World) ->
    % Rate at which power is generated
    WithPower = match_subcomponents(power, ID, World),
    F0 = fun(ComponentList, AccIn) ->
        Pow = ow_ecs:get(power, ComponentList),
        Pow + AccIn
    end,
    Power = lists:foldl(F0, 0, WithPower),
    WithCapacity = match_subcomponents(energy_capacity, ID, World),
    F1 = fun(ComponentList, AccIn) ->
        Cap = ow_ecs:get(energy_capacity, ComponentList),
        Cap + AccIn
    end,
    Capacity = lists:foldl(F1, 0, WithCapacity),
    Current =
        if
            Cur > Capacity ->
                Capacity;
            true ->
                Cur
        end,
    #{
        cur_reactor => Current,
        max_reactor => Capacity,
        rate_reactor => Power
    }.

thrust(ID, World) ->
    WithThrust = match_subcomponents(thrust, ID, World),
    F = fun(ComponentList, AccIn) ->
        Thrust = ow_ecs:get(thrust, ComponentList),
        Orientation = ow_ecs:get(orientation, ComponentList),
        RotatedThrust = rotate_ccw(Thrust, Orientation),
        lists:zipwith(fun(X, Y) -> X + Y end, RotatedThrust, AccIn)
    end,
    lists:foldl(F, [0, 0, 0, 0], WithThrust).

map(_Grid) ->
    ok.

%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------

instance_module(Coords, Type, Orientation, ParentID, Query) ->
    % Get the default configuration from file
    DefaultModules = ?DEFAULT_MODULES,
    % Components added at instance time
    Components = [
        {grid_coords, Coords},
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

rotate_cw(List, N) when N < 0 ->
    rotate_ccw(List, -N);
rotate_cw(List, 0) ->
    List;
rotate_cw([Head | Tail], Rotations) ->
    rotate_cw(Tail ++ [Head], Rotations - 1).

rotate_ccw(List, N) when N < 0 ->
    rotate_cw(List, -N);
rotate_ccw(List, 0) ->
    List;
rotate_ccw(List, Rotations) ->
    [Head | Tail] = lists:reverse(List),
    rotate_ccw([Head | lists:reverse(Tail)], Rotations - 1).
