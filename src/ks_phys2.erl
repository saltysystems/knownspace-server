-module(ks_phys2).

-export([proc_phys/2]).

-type translation() :: 'IMPULSE_FWD' | 'IMPULSE_REV' | 'IMPULSE_LEFT' | 'IMPULSE_RIGHT'.
-type rotation() :: 'ROTATE_LEFT' | 'ROTATE_RIGHT'.
-type vector() :: ow_vector:vector().
-type kinematics() :: #{
    pos_t := vector(),
    vel_t := vector(),
    pos_r := number(),
    vel_r => number()
}.

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

-spec proc_phys(term(), ow_ecs2:world()) -> ok.
proc_phys(ZoneData, World) ->
    % Get one input for each connected player
    InputList = ks_input_comp:pop(),
    case InputList of
        [] -> ok;
        Else -> logger:notice("Popped some input: ~p", [Else])
    end,
    % Apply the input
    [ apply_input(Input, ZoneData, World) || Input <- InputList ],
    % Update the world state across all actors, not just ones with input
    Actors = ow_ecs2:match_components([kinematics], World),
    [ simulate_frame(Actor, ZoneData, World) || Actor <- Actors ].

simulate_frame({ID, Components}, ZoneData, World) ->
    Kinematics = ow_ecs2:get(kinematics, Components),
    Kinematics2 = update_translation(Kinematics, ZoneData),
    Kinematics3 = update_rotation(Kinematics2, ZoneData),
    ow_ecs2:add_component(kinematics, Kinematics3, ID, World).

apply_input({SessionID, InputMsg}, _ZoneData, World) ->
    % Get ship properties
    Components = ow_ecs2:entity(SessionID, World),
    Kinematics = ow_ecs2:get(kinematics, Components),
    Torque = ow_ecs2:get(torque, Components),
    Mass = ow_ecs2:get(mass, Components),
    AngularMass = ow_ecs2:get(angular_mass, Components),
    Thrust = ow_ecs2:get(thrust, Components),
    % Get the keys for this input
    #{ keys := Keys } = InputMsg,
    % Ensure that the input keys are unique for this frame
    UniqKeys = lists:uniq(Keys),
    % Fold over the kinematics for each key this frame
    F = fun(Key,KinematicsIn) ->
            case Key of
                Key when Key == 'IMPULSE_FWD';
                         Key == 'IMPULSE_REV';
                         Key == 'IMPULSE_LEFT';
                         Key == 'IMPULSE_RIGHT' -> 
                    apply_translation(Key, KinematicsIn, Mass, Thrust);
                Key when Key == 'ROTATE_LEFT';
                         Key == 'ROTATE_RIGHT' ->
                    apply_rotation(Key, KinematicsIn, AngularMass, Torque)
            end
        end,
    UpdatedKinematics = lists:foldl(F, Kinematics, UniqKeys),
    % Store the updated kinematics
    ow_ecs2:add_component(kinematics, UpdatedKinematics, SessionID, World),
    % Return the ID of the actor 
    SessionID.


-spec apply_translation(translation(), kinematics(), integer(), integer()) -> kinematics().
apply_translation(Direction, Kinematics, Mass, Thrust) ->
    % Apply an impulse in a direction applies an acceleration vector in the
    % selected direction
    %[LThrust, BThrust, RThrust, TThrust] = Thrust,
    #{ left := LThrust,
       bottom := BThrust,
       right := RThrust,
       top := TThrust
     } = Thrust,
    % Get the current velocity, acceleration and rotation
    #{acc_t := {Xa, Ya}, pos_r := Rot} = Kinematics,
    Acc =
        case Direction of
            % The direction of movement is opposite the thrust direction
            'IMPULSE_FWD' ->
                {0.0 * BThrust / Mass, 1.0 * BThrust / Mass};
            'IMPULSE_REV' ->
                {0.0 * TThrust / Mass, -1.0 * TThrust / Mass};
            'IMPULSE_LEFT' ->
                {1.0 * RThrust / Mass, 0.0 * RThrust / Mass};
            'IMPULSE_RIGHT' ->
                {-1.0 * LThrust / Mass, 0.0 * LThrust / Mass}
        end,
    % Rotate the keyed vector by the current rotation
    {Xa2, Ya2} = ow_vector:rotate(Acc, Rot),
    % Subtract (negative Y is up) from the original vector
    Acc1 = {Xa - Xa2, Ya - Ya2},
    Kinematics#{acc_t := Acc1}.

-spec apply_rotation(rotation(), kinematics(), float(), integer()) -> kinematics().
apply_rotation(Direction, Kinematics, AngularMass, Torque) ->
    #{acc_r := AccR} = Kinematics,
    SpinDirection =
        case Direction of
            'ROTATE_RIGHT' -> 1.0;
            'ROTATE_LEFT' -> -1.0
        end,
    AccR1 = SpinDirection * (Torque / AngularMass),
    Kinematics#{acc_r := AccR + AccR1}.

update_translation(Kinematics, ZoneData) ->
    #{tick_ms := TickMs, env := Env} = ZoneData,
    #{max_vel_t := MaxVel, acc_factor := AccFactor} = Env,
    #{pos_t := Pos0, vel_t := Vel0, acc_t := Acc0} = Kinematics,
    DeltaT = TickMs / 1000,
    % Scale acceleration by the acceleration factor
    Acc1 = ow_vector:scale(Acc0, AccFactor),
    % Calculate the new velocity
    % v = v0 + a*t
    Vel1 = ow_vector:add(Vel0, ow_vector:scale(Acc1, DeltaT)),
    % Fix the velocity to the environment maximum
    Vel2 =
        case ow_vector:length_squared(Vel1) >= math:pow(MaxVel, 2) of
            true ->
                ow_vector:scale(ow_vector:normalize(Vel1), MaxVel);
            false ->
                Vel1
        end,
    Pos1 = ow_vector:add(Pos0, ow_vector:scale(Vel2, DeltaT)),
    % Update the velocity, position
    % Then set acceleration back to 0 in preparation for the next frame
    Kinematics#{
        pos_t := Pos1,
        vel_t := Vel2,
        acc_t := {0.0, 0.0}
    }.

-spec update_rotation(map(), ow_ecs2:world()) -> ok.
update_rotation(Kinematics, ZoneData) ->
    % Get the zone data
    #{tick_ms := TickMs, env := Env} = ZoneData,
    #{max_vel_r := MaxVel} = Env,
    #{pos_r := Pos0, vel_r := Vel0, acc_r := Acc0} = Kinematics,
    DeltaT = TickMs / 1000,
    % TODO: Scale the rotation by the rotation factor
    % AngAcc1 = Acc1 * RotAccFactor 
    Vel1 = Vel0 + Acc0 * DeltaT,
    Vel2 =
        case Vel1 of
            _ when Vel1 < -MaxVel ->
                -MaxVel;
            _ when Vel1 > MaxVel ->
                MaxVel;
            _ ->
                Vel1
        end,
    Pos1 = Pos0 + Vel2 * DeltaT,
    Kinematics#{pos_r := Pos1, vel_r := Vel2, acc_r := 0.0}.
