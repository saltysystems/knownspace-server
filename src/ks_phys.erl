-module(ks_phys).

-export([proc_phys/2, apply/4]).

-type direction() :: fwd | rev | left | right.
-type move() :: impulse | rotate.
-type vector() :: ow_vector:vector().
-type kinematics() :: #{
    pos_t := vector(),
    vel_t := vector(),
    pos_r := number(),
    vel_r => number()
}.

-spec key_table() -> list().
key_table() ->
    [
        {'IMPULSE_FWD', fun(ID, _, W) -> apply(impulse, fwd, ID, W) end},
        {'IMPULSE_REV', fun(ID, _, W) -> apply(impulse, rev, ID, W) end},
        {'IMPULSE_LEFT', fun(ID, _, W) -> apply(impulse, left, ID, W) end},
        {'IMPULSE_RIGHT', fun(ID, _, W) -> apply(impulse, right, ID, W) end},
        {'ROTATE_LEFT', fun(ID, _, W) -> apply(rotate, left, ID, W) end},
        {'ROTATE_RIGHT', fun(ID, _, W) -> apply(rotate, right, ID, W) end}
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFINITIONS                                                         %
% ------------------------------------------------------------------- %
% Property     | Linear    | Rotational                               %
% ------------------------------------------------------------------- %
% Position     | pos_t     | pos_r                                    %
% Velocity     | vel_t     | vel_r                                    %
% Acceleration | acc_t     | acc_r                                    %
% Mass         | mass      | angular_mass                             %
% Force        | thrust    | torque                                   %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec proc_phys(ow_ecs2:world(), term()) -> ok.
proc_phys(ZoneData, World) ->
    % Match the input and components for any changes this tick
    Actors = ow_ecs2:match_components([input, kinematics], World),
    % Apply the input for any player who has the input component, then zero out
    % the component.
    process_actors(Actors, ZoneData, World),
    % Get the entities and update rotations
    PhysEntities = ow_ecs2:match_component(kinematics, World),
    update_rotation(PhysEntities, ZoneData, World),
    % Then do it again for positions
    PhysEntities1 = ow_ecs2:match_component(kinematics, World),
    update_position(PhysEntities1, ZoneData, World).

% Recurse over all of the actors that match the component
-spec process_actors([ow_ecs2:entity()], map(), ow_ecs2:world()) -> ok.
process_actors([], _ZoneData, _World) ->
    ok;
process_actors([{ID, Components} | Rest], ZoneData, World) ->
    InputList = ow_ecs2:get(input, Components),
    ks_input:apply(InputList, ID, key_table(), World),
    process_actors(Rest, ZoneData, World).

-spec apply(move(), direction(), ks_actor:actor(), ow_ecs2:world()) -> ok.
apply(Type, Direction, ID, World) ->
    Components = ow_ecs2:entity(ID, World),
    Kinematics = ow_ecs2:get(kinematics, Components),
    Torque = ow_ecs2:get(torque, Components),
    AngularMass = ow_ecs2:get(angular_mass, Components),
    Thrust = ow_ecs2:get(thrust, Components),
    Mass = ow_ecs2:get(mass, Components),
    UpdatedKinematics =
        case Type of
            impulse ->
                apply_impulse(Direction, Kinematics, Mass, Thrust);
            rotate ->
                % TODO: Add MaxRot
                apply_rotation(Direction, Kinematics, AngularMass, Torque)
        end,
    ow_ecs2:add_component(kinematics, UpdatedKinematics, ID, World).

-spec apply_impulse(direction(), kinematics(), integer(), integer()) -> kinematics().
apply_impulse(Direction, Kinematics, Mass, Thrust) ->
    % Apply an impulse in a direction increases or decreases the velocity
    % vector by a fixed amount.
    <<LThrust:8, BThrust:8, RThrust:8, TThrust:8>> = Thrust,
    % Get the current velocity
    #{vel_t := {Xv, Yv}, pos_r := Rot} = Kinematics,
    logger:notice("Velocity is: ~p", [{Xv, Yv}]),
    Vel1 =
        case Direction of
            % The direction of movement is opposite the thrust direction
            % Fwd = Bottom Thruster
            % Rev = Top thruster
            % Left = Right thruster
            % Right = Left thruster
            fwd ->
                {0 * BThrust/Mass, 1 * BThrust/Mass};
            rev ->
                {0 * TThrust/Mass, -1 * TThrust/Mass};
            left ->
                {-1 * RThrust/Mass, 0 * RThrust/Mass};
            right ->
                {1 * LThrust/Mass, 0 * LThrust/Mass}
        end,
    % Rotate the keyed vector by the current rotation
    {Xv2, Yv2} = ow_vector:rotate(Vel1, Rot),
    % Subtract (negative Y is up) from the original vector
    Vel3 = {Xv - Xv2, Yv - Yv2},
    Kinematics#{vel_t := Vel3}.

-spec apply_rotation(left | right, kinematics(), float(), integer()) -> kinematics().
apply_rotation(Direction, Kinematics, AngularMass, Torque) ->
    #{vel_r := VelR} = Kinematics,
    D =
        case Direction of
            right -> 1.0;
            left -> -1.0
        end,
    VelR1 = VelR + D * (Torque / AngularMass),
    Kinematics#{vel_r := VelR1}.

-spec update_rotation([ow_ecs2:entity()], term(), ow_ecs2:world()) -> ok.
update_rotation([], _ZoneData, _World) ->
    ok;
update_rotation([{ID, Components} | R], ZD = #{tick_ms := TickMs}, World) ->
    % Now apply the physics
    Kinematics = ow_ecs2:get(kinematics, Components),
    #{pos_r := RotP, vel_r := RotV, acc_r := RotA} = Kinematics,
    DeltaT = TickMs / 1000,
    RotV1 = RotV + RotA * DeltaT,
    RotP1 = RotP + RotV1 * DeltaT,
    KinematicsUpdate = Kinematics#{pos_r := RotP1, vel_r := RotV1},
    ow_ecs2:add_component(kinematics, KinematicsUpdate, ID, World),
    update_rotation(R, ZD, World).

-spec update_position([ow_ecs2:entity()], term(), ow_ecs2:world()) -> ok.
update_position([], _ZoneData, _World) ->
    ok;
update_position([{ID, Components} | R], ZoneData, World) ->
    #{tick_ms := TickMs, env := Env} = ZoneData,
    #{max_vel_t := MaxVel} = Env,
    % Now apply the physics
    Kinematics = ow_ecs2:get(kinematics, Components),
    #{pos_t := Pos, vel_t := Vel} = Kinematics,
    DeltaT = TickMs / 1000,
    % Fix the velocity to the environment maximum
    Vel1 =
        case ow_vector:length_squared(Vel) > math:pow(MaxVel, 2) of
            true ->
                ow_vector:scale(ow_vector:normalize(Vel), MaxVel);
            false ->
                Vel
        end,
    Pos1 = ow_vector:add(Pos, ow_vector:scale(Vel1, DeltaT)),
    KinematicsUpdate = Kinematics#{pos_t := Pos1, vel_t := Vel1},
    ow_ecs2:add_component(kinematics, KinematicsUpdate, ID, World),
    update_position(R, ZoneData, World).
