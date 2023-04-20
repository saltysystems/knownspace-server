-module(ks_visibility).
-export([proc_target_info/2]).

% General algorithm:
%  1. Is the target within distance?
%  2. Determine what modules are within our vision cone by checking the angle
%     of a potential ray against the center of mass for the module
%  3. Discard any modules that are occluded

proc_target_info(ZoneData, World) ->
    % For every entity with kinematics, 
    Actors = ow_ecs2:match_components([kinematics, target], World),
    F = fun({_ID, ActorComponents}) ->
            Target = ow_ecs:get(target, ActorComponents),
            case in_range(ActorComponents, Target, ZoneData) of
                {true, TComponents} -> 
                    visible_modules(ActorComponents, TComponents);
                    %Modules = visible_modules(ID, TComponents),
                    %% Assumes the caller of proc is the zone process
                    %Msg = #{ grid_coords => Modules },
                    %ow_zone:send(self(), [ID], {target_info, Msg})
                false -> ok
            end
        end,
    lists:foreach(F, Actors).

in_range(Components, TargetID, #{ max_visibility := MaxVis }) ->
    #{ pos_t := ActorPos } = ow_ecs2:get(kinematics, Components),
    % entity may not exist
    case ow_ecs2:entity(TargetID) of 
        false -> false;
        TargetComponents -> 
            TargetKinematics = ow_ecs2:get(kinematics, TargetComponents),
            #{ pos_t := TargetPos } = TargetKinematics,
            {(TargetPos - ActorPos) < MaxVis, TargetComponents}
    end.

visible_modules(_ActorComponents, TargetComponents) ->
    % Get the visible components of the target
    % TODO: Calculate.
    % For now - just list all of the components.
    ShipGrid = ow_ecs2:get(shipgrid, TargetComponents),
    logger:notice("ShipGrid is: ~p", [ShipGrid]).
