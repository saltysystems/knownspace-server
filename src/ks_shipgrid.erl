-module(ks_shipgrid).

% @doc
% The ShipGrid module provides an interface for calculating various Group
% components as used by the actor module
% @end

-export([new/2, del/2, add/5, match_subcomponents/3, map_cells/2, cell/3, netformat/2]).

-include("modules.hrl").
% px
-define(CELL_SIZE, 32).

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
    % Clean up any old components
    del(ID, World),
    % Add the new, clean component
    ow_ecs:add_component(shipgrid, Grid, ID, World).

-spec del(integer(), atom()) -> ok.
del(ID, World) ->
    case ow_ecs:try_component(shipgrid, ID, World) of
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
                        ow_ecs:rm_entity(ChildID, World)
                    end,
                    lists:foreach(F, Children),
                    ow_ecs:del_component(children, ID, World)
            end,
            % Remove the shipgrid
            ow_ecs:del_component(shipgrid, ID, World)
    end.

-spec add(vector(), atom(), rotation(), id(), atom()) -> ok.
add(Coords, Type, Rotation, ParentID, World) ->
    case ow_ecs:try_component(shipgrid, ParentID, World) of
        false ->
            {error, no_grid};
        Data ->
            Grid = ow_ecs:get(shipgrid, Data),
            Children = ow_ecs:get(children, Data, []),
            % Instance the new child module and update the grid
            ChildRef = instance_module(Coords, Type, Rotation, ParentID, World),
            Children2 = [ChildRef | Children],
            Grid2 = ow_sparsegrid:put(Coords, ChildRef, Grid),
            % Update the entity
            Components = [
                {shipgrid, Grid2},
                {children, Children2}
            ],
            ow_ecs:add_components(Components, ParentID, World),
            % Recalculate pivot point, thrust, etc. This may need to be a fun
            % with callbacks as it grows. Can't predict what properties need to
            % be present at the macro level
            Pivot = pivot(ParentID, World),
            % Get the current reactor stats before updating
            CurReactor = ow_ecs:get(reactor, Data, ks_reactor:new()),
            Reactor = reactor(CurReactor, ParentID, World),
            % Calculate the thrust parameters for each cardinal direction
            Thrust = thrust(ParentID, World),
            % Calculate the torque provided by the gyroscope.
            Torque = torque(ParentID, World),
            % Calculate the moment of inertia
            AngularMass = angular_mass(Pivot, ParentID, World),
            % Calculate the ship's hull for collision detection operations. 
            Hull = hull(ParentID, World),
            % Pass another update to the entity with derived properties
            Components2 = [
                {pivot, Pivot},
                {reactor, Reactor},
                {thrust, Thrust},
                {angular_mass, AngularMass},
                {torque, Torque},
                {hull, Hull}
            ],
            ow_ecs:add_components(Components2, ParentID, World)
    end.
% I'd like to come up with a good boundary where the side-effecty stuff is
% wrapped..
%E = ow_ecs:entity(ParentID, World),
%FinalGrid = ow_ecs:get(shipgrid, E),
%{FinalGrid, ParentID, World}.

match_subcomponents(Component, ParentID, World) ->
    case ow_ecs:try_component(children, ParentID, World) of
        % no children, so the match is always emptylist
        false ->
            [];
        Data ->
            Children = ow_ecs:get(children, Data),
            F =
                fun(ChildID) ->
                    case ow_ecs:try_component(Component, ChildID, World) of
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

angular_mass(Pivot, ID, World) ->
    % Calculate the moment of inertia (I) for a given ship by calculating it
    % for each component and then summing
    % --
    % HACK for the moment - just calculate based on things with hitboxes
    % IF we add proper mass, then change it.
    WithHitboxes = match_subcomponents(hitbox, ID, World),
    F = fun(ComponentList, AccIn) ->
        % Get the cell position
        GridCoords = ow_ecs:get(grid_coords, ComponentList),
        {Xd, Yd} = ow_vector:subtract(Pivot, GridCoords),
        % Assume the cell is a point mass and we calculate the angular mass
        % around the pivot point
        % Then the moment of inertia is:
        % I = m*r^2
        % Distance for a 
        % r = sqrt(X^2+Y^2)
        % r^2 = X^2 + Y^2
        % let R = r^2
        % If M=1, then I = R
        % --
        % So first calculate R by subtracting the current position from the
        % center of mass
        R = ow_vector:subtract(Pivot, GridCoords),
        I = 
            case R == {0,0} of
                true -> 
                    % If R = 0, then this object is on top of the pivot point
                    % exactly (single cell?)
                    % Assume M=1,L=1,W=1
                    % I = (1/12) * 1 * (1^2 + 1^2)
                    (1/6);
                false ->
                    % Get the constituent components and calculate the moment
                    % of inertia
                    {Xd,Yd} = R,
                    math:pow(Xd,2) + math:pow(Yd,2)
            end,
        I + AccIn
    end,
    lists:foldl(F, 0, WithHitboxes).


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

reactor(#{now := Cur}, ID, World) ->
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
        now  => Current,
        max  => Capacity,
        rate => Power
    }.

thrust(ID, World) ->
    WithThrust = match_subcomponents(thrust, ID, World),
    F = fun(ComponentList, AccIn) ->
        Thrust = ow_ecs:get(thrust, ComponentList),
        Orientation = ow_ecs:get(orientation, ComponentList),
        RotatedThrust = rotate_ccw(Thrust, Orientation),
        TempList = binary:bin_to_list(RotatedThrust),
        lists:zipwith(fun(X, Y) -> X + Y end, TempList, AccIn)
    end,
    CalculatedThrust = lists:foldl(F, [0, 0, 0, 0], WithThrust),
    binary:list_to_bin(CalculatedThrust).

% TODO: write a generic "sum component" fun?
torque(ID, World) ->
    WithTorque = match_subcomponents(torque, ID, World),
    F = fun(ComponentList, AccIn) ->
        ow_ecs:get(torque, ComponentList) + AccIn
    end,
    lists:foldl(F, 0, WithTorque).

map_cells(ID, World) ->
    % Get child IDs
    % Return the cell (in network format)
    % Get the shipgrid
    ComponentList = ow_ecs:entity(ID, World),
    Children = ow_ecs:get(children, ComponentList),
    % For each child, get the cell
    F = fun(ChID, Acc) ->
                ChildComp = ow_ecs:entity(ChID, World),
                M = maps:from_list(ChildComp),
                M1 = M#{ id => ChID },
                [ ow_vector:to_proto(M1) | Acc ]
        end,
    lists:foldl(F, [], Children).

cell({X,Y}, ID, World) ->
    % Return the cell (in network format)
    % Get the shipgrid
    {ID, ComponentList} = ow_ecs:entity(ID, World),
    ShipGrid = ow_ecs:get(shipgrid, ComponentList),
    % Get the cell
    Cell = maps:get({X,Y}, ShipGrid),
    % Get the child ID
    #{ data := ChildID } = Cell,
    % Get the child entity
    {ChildID, ChComponentList} = ow_ecs:entity(ChildID, World),
    ChComponentList.


netformat(ID, World) ->
    Components = ow_ecs:entity(ID, World),
    Phys = ow_ecs:get(phys, Components),
    Pivot = ow_vector:vector_map(ow_ecs:get(pivot, Components)),
    Reactor = ow_ecs:get(reactor, Components),
    Thrust = ow_ecs:get(thrust, Components),
    Torque = ow_ecs:get(torque, Components),
    AngularMass = ow_ecs:get(angular_mass, Components),
    Cells = map_cells(ID, World),
    #{
      phys => Phys,
      pivot => Pivot,
      torque => Torque,
      angular_mass => AngularMass,
      reactor => Reactor,
      thrust => Thrust,
      cells => Cells
     }.

%%-------------------------------------------------------------------
%% Internal functions
%%-------------------------------------------------------------------

instance_module(Coords, Type, Orientation, ParentID, World) ->
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
    ow_ecs:add_components(Components ++ DefaultComponents, ModuleID, World),
    % Return the child ID back to the caller
    ModuleID.

rotate_cw(List, N) when N < 0 ->
    rotate_ccw(List, -N);
rotate_cw(List, 0) ->
    List;
rotate_cw(<<H:8, T/binary>>, Rotations) ->
    rotate_cw(<<T/binary, H:8>>, Rotations - 1).

rotate_ccw(List, N) when N < 0 ->
    rotate_cw(List, -N);
rotate_ccw(List, 0) ->
    List;
rotate_ccw(Bin, Rotations) ->
    << Head:8,  Tail/binary >> = rev(Bin),
    Rev = rev(Tail),
    rotate_ccw(<< Head, Rev/binary >>, Rotations - 1).


%  from
%  https://stackoverflow.com/questions/20830201/better-way-to-reverse-binary
rev(Binary) ->
   Size = erlang:bit_size(Binary),
   <<X:Size/integer-little>> = Binary,
   <<X:Size/integer-big>>.
