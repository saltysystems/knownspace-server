-module(ks_shipgrid).

% @doc
% The ShipGrid module provides an interface for calculating various Group
% components as used by the actor module
% @end

-export([
    new/5,
    new_grid/2,
    del_grid/2,
    add/5,
    match_subcomponents/3,
    subcomponent_pos_t/2,
    map_cells/2,
    cell/3,
    netformat/2,
    get/2
]).

-include("modules.hrl").

-opaque shipgrid() :: {map(), integer(), atom()}.
-export_type([shipgrid/0]).

-type vector() :: ow_vector:vector().
-type rotation() :: 0 | 1 | 2 | 3.
-type id() :: integer().

%%-------------------------------------------------------------------
%% Public API
%%-------------------------------------------------------------------
%%
new(Coordinates, Type, ID, Handle, World) ->
    new_grid(ID, World),
    erlang:apply(ks_ship, Type, [ID, World]),
    % Now add the other required pieces
    ow_ecs2:add_component(handle, Handle, ID, World),
    ow_ecs2:add_component(actor, true, ID, World),
    Kinematics =
        #{
            pos_t => Coordinates,
            vel_t => {0.0, 0.0},
            acc_t => {0.0, 0.0},
            pos_r => 0.0,
            vel_r => 0.0,
            acc_r => 0.0
        },
    ow_ecs2:add_component(kinematics, Kinematics, ID, World),
    % Return the final grid
    ow_ecs2:entity(ID, World).

-spec new_grid(integer(), atom()) -> ok.
new_grid(ID, World) ->
    Grid = ow_sparsegrid:new(),
    % Clean up any old components
    del_grid(ID, World),
    % Add the new, clean component
    ow_ecs2:add_component(shipgrid, Grid, ID, World).

-spec del_grid(integer(), atom()) -> ok.
del_grid(ID, World) ->
    case ow_ecs2:try_component(shipgrid, ID, World) of
        false ->
            ok;
        Components ->
            % Get the children and clean them up.
            Children = ow_ecs2:get(children, Components),
            case Children of
                false ->
                    ok;
                _ ->
                    F = fun(ChildID) ->
                        ow_ecs2:rm_entity(ChildID, World)
                    end,
                    lists:foreach(F, Children),
                    ow_ecs2:del_component(children, ID, World)
            end,
            % Remove the shipgrid
            ow_ecs2:del_component(shipgrid, ID, World)
    end.

-spec add(vector(), atom(), rotation(), id(), atom()) -> ok.
add(Coords, Type, Rotation, ParentID, World) ->
    case ow_ecs2:try_component(shipgrid, ParentID, World) of
        false ->
            {error, no_grid};
        Data ->
            Grid = ow_ecs2:get(shipgrid, Data),
            Children = ow_ecs2:get(children, Data, []),
            % Instance the new child module and update the grid
            ChildRef = instance_module(Coords, Type, Rotation, ParentID, World),
            Children2 = [ChildRef | Children],
            Grid2 = ow_sparsegrid:put(Coords, ChildRef, Grid),
            % Update the entity
            Components = [
                {shipgrid, Grid2},
                {children, Children2}
            ],
            ow_ecs2:add_components(Components, ParentID, World),
            % Recalculate center of mass, thrust, etc. This may need to be a
            % fun with callbacks as it grows. Can't predict what properties
            % need to be present at the macro level
            % Calculate the thrust parameters for each cardinal direction
            Thrust = thrust(ParentID, World),
            % Calculate the torque provided by the gyroscope.
            Torque = torque(ParentID, World),
            % Calculate the ship's hull for collision detection operations.
            Hull = graham_hull(ParentID, World),
            FullHull = full_hull(ParentID, World),
            % Calculate the moments (mass, center of mass, angular mass)
            Mass = mass(ParentID, World),
            CenterOfMass = center_of_mass(ParentID, World),
            AngularMass = moment_of_inertia(CenterOfMass, ParentID, World),
            % Pass another update to the entity with derived properties
            Components2 = [
                {thrust, Thrust},
                {mass, Mass},
                {center_of_mass, CenterOfMass},
                {angular_mass, AngularMass},
                {torque, Torque},
                {hull, Hull},
                {full_hull, FullHull}
            ],
            ow_ecs2:add_components(Components2, ParentID, World)
    end.
% I'd like to come up with a good boundary where the side-effecty stuff is
% wrapped..
%E = ow_ecs2:entity(ParentID, World),
%FinalGrid = ow_ecs2:get(shipgrid, E),
%{FinalGrid, ParentID, World}.

subcomponent_pos_t(ChildComponents, World) ->
    ParentID = ow_ecs2:get(parent, ChildComponents),
    ParentComponents = ow_ecs2:entity(ParentID, World),
    Kinematics = ow_ecs2:get(kinematics, ParentComponents),
    #{ pos_t := Pos } = Kinematics,
    % Translate the module by its cell position
    Cell = ow_ecs2:get(grid_coords, ChildComponents),
    ScaledCell = ow_vector:scale(Cell, ?CELL_SIZE),
    [SubcomponentPos] = ow_vector:translate([ScaledCell], Pos),
    SubcomponentPos.


match_subcomponents(Component, ParentID, World) ->
    case ow_ecs2:try_component(children, ParentID, World) of
        % no children, so the match is always emptylist
        false ->
            [];
        Data ->
            Children = ow_ecs2:get(children, Data),
            F =
                fun(ChildID) ->
                    case ow_ecs2:try_component(Component, ChildID, World) of
                        false ->
                            false;
                        ChildData ->
                            % Insert the child ID into the data
                            {true, [{id, ChildID} | ChildData]}
                    end
                end,
            lists:filtermap(F, Children)
    end.

center_of_mass(ID, World) ->
    % Calculate the center of mass by first calculating the CoM of each
    % module, then summing the sums. Assume each module has uniform mass
    % density throughout, but mass may differ per module.
    %
    % Assume each cell to be 10px by 10 px, such that a unit rectangle spanning
    % the area is defined as [{-5,-5},{5,-5},{5,5},{-5,5}]
    WithHitboxes = match_subcomponents(hitbox, ID, World),
    % Unzip the vectors
    F = fun(ComponentList, {CenterIn, TotalMassIn}) ->
        % Get the hitbox and cell
        Hitbox = ow_ecs2:get(hitbox, ComponentList),
        % 0 if undefined
        Mass = ow_ecs2:get(mass, ComponentList, 0),
        % Calculate a center of mass for the module by summing its columns and
        % rows.
        {XList, YList} = lists:unzip(Hitbox),
        ModuleCoM = {lists:sum(XList), lists:sum(YList)},
        % Translate the module by its cell position
        Cell = ow_ecs2:get(grid_coords, ComponentList),
        ScaledCell = ow_vector:scale(Cell, ?CELL_SIZE),
        [NormalizedCoM] = ow_vector:translate([ModuleCoM], ScaledCell),
        % Multiply by the mass of the component
        CoM = ow_vector:scale(NormalizedCoM, Mass),
        % Add the CoM and mass to the running totals
        {ow_vector:add(CoM, CenterIn), Mass + TotalMassIn}
    end,
    {CoM, TotalMass} = lists:foldl(F, {{0, 0}, 0}, WithHitboxes),
    ow_vector:scale(CoM, 1 / TotalMass).

moment_of_inertia(CoM, ID, World) ->
    % Calculate the moment of inertia (I) for a given ship by calculating it
    % for each component and then summing
    WithHitboxes = match_subcomponents(hitbox, ID, World),
    F = fun(ComponentList, AccIn) ->
        % Get the mass
        Mass = ow_ecs2:get(mass, ComponentList, 0),
        % Get the cell position
        GridCoords = ow_ecs2:get(grid_coords, ComponentList),
        {Xd, Yd} = ow_vector:subtract(CoM, GridCoords),
        % Assume the cell is a point mass and we calculate the angular mass
        % around the pivot point
        % Then the moment of inertia is:
        % I = m*r^2
        % Distance for a
        % r = sqrt(X^2+Y^2)
        % r^2 = X^2 + Y^2
        % R = r^2
        % --
        % So first calculate R by subtracting the current position from the
        % center of mass
        % First check if the object is directly on the center of mass.
        R = ow_vector:subtract(CoM, GridCoords),
        I =
            case R == {0, 0} of
                true ->
                    % If R = 0, then this object is on top of the center of
                    % mass exactly. But I think it would be wierd and broken if
                    % it were just 0? So let's fudge it.
                    Mass;
                false ->
                    % Get the constituent components and calculate the moment
                    % of inertia
                    {Xd, Yd} = R,
                    Mass * (math:pow(Xd, 2) + math:pow(Yd, 2))
            end,
        I + AccIn
    end,
    round(lists:foldl(F, 0, WithHitboxes)).

mass(ID, World) ->
    WithMass = match_subcomponents(mass, ID, World),
    F = fun(ComponentList, AccIn) ->
        Mass = ow_ecs2:get(mass, ComponentList),
        Mass + AccIn
    end,
    lists:foldl(F, 0, WithMass).

graham_hull(ID, World) ->
    % This function calculates an outer hull of a collection of 2D polygons
    % using Graham's scan method by decomposing them into a list of vertices
    % and then eliminating cavities.
    % --
    % Grab all components on the ship which have a hitbox
    WithHitboxes = match_subcomponents(hitbox, ID, World),
    % Get all vertices for all hitboxes
    F = fun(ComponentList, AccIn) ->
        % Get the hitbox
        Hitbox = ow_ecs2:get(hitbox, ComponentList),
        % Scale the cell by px-per-cell
        Cell = ow_ecs2:get(grid_coords, ComponentList),
        ScaledCell = ow_vector:scale(Cell, ?CELL_SIZE),
        % Translate the hitbox by the scaled position
        THB = ow_vector:translate(Hitbox, ScaledCell),
        % Add the vertices to the list
        [THB | AccIn]
    end,
    DeepVertices = lists:foldl(F, [], WithHitboxes),
    Vertices = lists:sort(lists:flatten(DeepVertices)),
    ow_vector:convex_hull(Vertices).

full_hull(ID, World) ->
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
        ChildID = ow_ecs2:get(id, ComponentList),
        % Get the hitbox
        Hitbox = ow_ecs2:get(hitbox, ComponentList),
        % Scale the cell by px-per-cell
        Cell = ow_ecs2:get(grid_coords, ComponentList),
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

thrust(ID, World) ->
    WithThrust = match_subcomponents(thrust, ID, World),
    F = fun(ComponentList, AccIn) ->
        Thrust = ow_ecs2:get(thrust, ComponentList),
        Orientation = ow_ecs2:get(orientation, ComponentList),
        RotatedThrust = rotate_ccw(Thrust, Orientation),
        TempList = RotatedThrust,
        lists:zipwith(fun(X, Y) -> X + Y end, TempList, AccIn)
    end,
    CalculatedThrust = lists:foldl(F, [0, 0, 0, 0], WithThrust),
    [Left,Bottom,Right,Top] = CalculatedThrust,
    #{ left => Left, 
       bottom => Bottom,
       right => Right, 
       top => Top
     }.

% TODO: write a generic "sum component" fun?
torque(ID, World) ->
    WithTorque = match_subcomponents(torque, ID, World),
    F = fun(ComponentList, AccIn) ->
        ow_ecs2:get(torque, ComponentList) + AccIn
    end,
    lists:foldl(F, 0, WithTorque).

map_cells(ID, World) ->
    % Get child IDs
    % Return the cell (in network format)
    % Get the shipgrid
    ComponentList = ow_ecs2:entity(ID, World),
    Children = ow_ecs2:get(children, ComponentList),
    % For each child, get the cell
    F = fun(ChID, Acc) ->
        ChildComp = ow_ecs2:entity(ChID, World),
        M = maps:from_list(ChildComp),
        M1 = M#{id => ChID},
        [ow_netfmt:to_proto(M1) | Acc]
    end,
    lists:foldl(F, [], Children).

cells(ID, World) ->
    % Get child IDs
    % Return the cell (in network format)
    % Get the shipgrid
    ComponentList = ow_ecs2:entity(ID, World),
    Children = ow_ecs2:get(children, ComponentList),
    % For each child, get the cell
    F = fun(ChID, Acc) ->
        ChildComp = ow_ecs2:entity(ChID, World),
        M = maps:from_list(ChildComp),
        M1 = M#{id => ChID},
        [M1 | Acc]
    end,
    lists:foldl(F, [], Children).

cell({X, Y}, ID, World) ->
    % Return the cell (in network format)
    % Get the shipgrid
    {ID, ComponentList} = ow_ecs2:entity(ID, World),
    ShipGrid = ow_ecs2:get(shipgrid, ComponentList),
    % Get the cell
    Cell = maps:get({X, Y}, ShipGrid),
    % Get the child ID
    #{data := ChildID} = Cell,
    % Get the child entity
    {ChildID, ChComponentList} = ow_ecs2:entity(ChildID, World),
    ChComponentList.

get(ID, World) ->
    % Not in network format. Defer to later.
    Components = ow_ecs2:entity(ID, World),
    Kinematics = ow_ecs2:get(kinematics, Components),
    Mass = ow_ecs2:get(mass, Components),
    AngularMass = ow_ecs2:get(angular_mass, Components),
    CenterOfMass = ow_ecs2:get(center_of_mass, Components),
    Thrust = ow_ecs2:get(thrust, Components),
    Torque = ow_ecs2:get(torque, Components),
    Hull = ow_ecs2:get(hull, Components),
    FullHull = ow_ecs2:get(full_hull, Components),
    Cells = cells(ID, World),
    #{
        kinematics => Kinematics,
        center_of_mass => CenterOfMass,
        angular_mass => AngularMass,
        mass => Mass,
        hull => Hull,
        full_hull => FullHull,
        torque => Torque,
        thrust => Thrust,
        cells => Cells
    }.

netformat(ID, World) ->
    Components = ow_ecs2:entity(ID, World),
    Kinematics = ow_netfmt:to_proto(ow_ecs2:get(kinematics, Components)),
    Mass = ow_ecs2:get(mass, Components),
    AngularMass = ow_ecs2:get(angular_mass, Components),
    CenterOfMass = ow_ecs2:get(center_of_mass, Components),
    Hull = ow_netfmt:vec2map(ow_ecs2:get(hull, Components)),
    Thrust = ow_ecs2:get(thrust, Components),
    Torque = ow_ecs2:get(torque, Components),
    Cells = map_cells(ID, World),
    #{
        kinematics => Kinematics,
        center_of_mass => CenterOfMass,
        angular_mass => AngularMass,
        mass => Mass,
        hull => Hull,
        torque => Torque,
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
    ow_ecs2:add_components(Components ++ DefaultComponents, ModuleID, World),
    % Return the child ID back to the caller
    ModuleID.

rotate_cw(List, N) when N < 0 ->
    rotate_ccw(List, -N);
rotate_cw(List, 0) ->
    List;
rotate_cw([H | T], Rotations) ->
    rotate_cw(T ++ [H], Rotations - 1).

rotate_ccw(List, N) when N < 0 ->
    rotate_cw(List, -N);
rotate_ccw(List, 0) ->
    List;
rotate_ccw(List, Rotations) ->
    [H|T] = lists:reverse(List),
    Rev = lists:reverse(T),
    rotate_ccw([H | Rev], Rotations - 1).
