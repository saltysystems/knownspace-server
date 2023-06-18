-module(ks_visibility).
-export([target_info/4, visible_modules/3]).

-include("modules.hrl").

%% Trivial visibility
% Just do a range check and anything within range is lit
trivial_target_info(SourceID, TargetID, ZoneEnv, World) ->
    case ow_ecs2:entity(SourceID, World) of
        % could be nonexistent by the time we get a response
        false -> 
            false;
        SourceComponents ->
            % Target could also be nonexistent by the time we get a response
            case in_range(SourceComponents, TargetID, ZoneEnv, World) of
                false -> false;
                {false, _} -> false;
                {true, TargetComponents} ->
                    TargetComponents %fixme
            end
    end.

%% Fancy visibility

% General algorithm:
%  1. Is the target within distance?
%  2. Determine what modules are within our vision cone 
%  3. Discard any modules that are occluded or not within the cone

target_info(SourceID, TargetID, ZoneEnv , World) ->
    % First get the ECS data for the actor
    %PotentialTargets = ow_ecs2:match_components([kinematics], World),
    % TODO: Switch to 'maybe' expression, maybe?
    case ow_ecs2:entity(SourceID, World) of
        % could be nonexistent by the time we get a response
        false -> 
            false;
        SourceComponents ->
            % Target could also be nonexistent by the time we get a response
            case in_range(SourceComponents, TargetID, ZoneEnv, World) of
                false -> false;
                {false, _} -> false;
                {true, TargetComponents} ->
                    visible_modules(SourceID, TargetComponents, World)
            end
    end.

in_range(Components, TargetID, #{max_visibility := MaxVis}, World) ->
    #{pos_t := ActorPos} = ow_ecs2:get(kinematics, Components),
    % entity may not exist
    case ow_ecs2:entity(TargetID, World) of
        false ->
            false;
        TargetComponents ->
            TargetKinematics = ow_ecs2:get(kinematics, TargetComponents),
            #{pos_t := TargetPos} = TargetKinematics,
            Distance = ow_vector:subtract(TargetPos, ActorPos),
            Length = ow_vector:length_squared(Distance),
            {Length < MaxVis, TargetComponents}
    end.

visible_modules(SourceID, TargetComponents, World) ->
    % For each of the actor's components with a weapon that has some arc, we
    % need to go through each weapon and calculate what modules are visible on
    % the target.
    % First, we we need to get the list of subcomponents on the actor that have firing arcs.
    WithArcs = ks_shipgrid:match_subcomponents(firing_arc, SourceID, World),
    SourceComponents = ow_ecs2:entity(SourceID, World),
    #{ pos_r := Rotation } = ow_ecs2:get(kinematics, SourceComponents),
    % For each of these components, we need to orient them in the global
    % coordinate space and cast a pair of rays corresponding to the leading and
    % trailing edges of the arc.
    F = fun(Child, Acc) -> 
            % TODO: Include parent rotation
            {BeginRay, EndRay} = edge_rays_from_arcs(Child),
            logger:notice("BeginRay: ~p", [BeginRay]),
            logger:notice("EndRay: ~p", [EndRay]),
            % Translate the rays by the ship's position + component position
            RealPos = ks_shipgrid:subcomponent_pos_t(Child, World),
            logger:notice("Real Pos: ~p", [RealPos]),
            [BRay] = ow_vector:translate([BeginRay], RealPos),
            [ERay] = ow_vector:translate([EndRay], RealPos),
            % Rotate the rays by the grid rotation
            RBRay = ow_vector:rotate(BRay, Rotation),
            RERay = ow_vector:rotate(ERay, Rotation),
            logger:notice("Adjusted BeginRay: ~p", [RBRay]),
            logger:notice("Adjusted EndRay: ~p", [RERay]),
            FullHull = ow_ecs2:get(full_hull, TargetComponents),
            logger:notice("FullHull: ~p", [FullHull]),
            TargetEdges = maps:keys(FullHull),
            logger:notice("Edges: ~p", [TargetEdges]),
            {Rays,Visible} = ow_vector:line_of_sight(RealPos, RERay, RBRay, TargetEdges),
            G = lists:uniq([ maps:get(Edge, FullHull) || Edge <- Visible ]),
            LoS = #{ rays => Rays, ray_origin => RealPos, modules => G }, 
            [ LoS | Acc ]
        end,
    [Results] = lists:foldl(F, [], WithArcs),
    % TODO: Fix for multiple source modules
    Results.
    


edge_rays_from_arcs(ModuleComponents) ->
    Arc = arc_to_radian(proplists:lookup(firing_arc, ModuleComponents)),
    % We want the edge rays to be on either side of the nominal ray, with the
    % nominal ray cutting them in the middle.
    HalfArc = Arc/2,
    % Orientation is CCW. 
    Orientation = proplists:lookup(orientation, ModuleComponents),
    NominalRay = case Orientation of
                     {orientation, 0} -> {0,-1}; % negative Y is up. 
                     {orientation, 1} -> {-1,0};
                     {orientation, 2} -> {0,1};
                     {orientation, 3} -> {1,0}
                 end,
    % Starting from the negative half arc, calculate the beginning edge
    BeginRay = ow_vector:rotate(NominalRay, -HalfArc),
    EndRay = ow_vector:rotate(NominalRay, HalfArc),
    {BeginRay, EndRay}.
    

arc_to_radian({firing_arc, 'ARC_360'}) ->
    2*math:pi(); % NOTE that the line-of-sight algo only handle up to 180deg
arc_to_radian({firing_arc, 'ARC_180'}) -> 
    math:pi();
arc_to_radian({firing_arc, 'ARC_90'}) ->
    math:pi()/2;
arc_to_radian({firing_arc, 'ARC_45'}) ->
    math:pi()/4;
arc_to_radian({firing_arc, 'ARC_22'}) ->
    math:pi()/8;
arc_to_radian({firing_arc, 'ARC_0'}) ->
    0.
