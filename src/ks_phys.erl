-module(ks_phys).
%
%-export([proc_phys/2, apply/5]).
%
%-type direction() :: fwd | rev | left | right.
%-type move() :: impulse | rotate.
%-type vector() :: ow_vector:vector().
%-type kinematics() :: #{
%    pos_t := vector(),
%    vel_t := vector(),
%    pos_r := number(),
%    vel_r => number()
%}.
%
%-spec key_table() -> list().
%key_table() ->
%    [
%        {'IMPULSE_FWD', fun(ID, _, ZD, W) -> apply(impulse, fwd, ID, ZD, W) end},
%        {'IMPULSE_REV', fun(ID, _, ZD, W) -> apply(impulse, rev, ID, ZD, W) end},
%        {'IMPULSE_LEFT', fun(ID, _, ZD, W) -> apply(impulse, left, ID, ZD, W) end},
%        {'IMPULSE_RIGHT', fun(ID, _, ZD, W) -> apply(impulse, right, ID, ZD, W) end},
%        {'ROTATE_LEFT', fun(ID, _, ZD, W) -> apply(rotate, left, ID, ZD, W) end},
%        {'ROTATE_RIGHT', fun(ID, _, ZD, W) -> apply(rotate, right, ID, ZD, W) end}
%    ].
%
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

%-spec proc_phys(ow_ecs2:world(), term()) -> ok.
%proc_phys(ZoneData, World) ->
%    % Match the input and components for any changes this tick
%    Actors = ow_ecs2:match_components([input, kinematics], World),
%    % Apply the input for any player who has the input component, then zero out
%    % the component.
%    process_actors(Actors, ZoneData, World),
%    % Get the entities and update rotations
%    PhysEntities = ow_ecs2:match_component(kinematics, World),
%    update_rotation(PhysEntities, ZoneData, World),
%    % Then do it again for positions
%    PhysEntities1 = ow_ecs2:match_component(kinematics, World),
%    update_position(PhysEntities1, ZoneData, World).
%proc_phys(ZoneData, World) ->
%    % For each player, play out any buffered input and simulate one frame
%    Actors = ow_ecs2:match_components([kinematics], World),
%    process_actors(Actors, ZoneData, World).

% Recurse over all of the actors that match the component
%-spec process_actors([ow_ecs2:entity()], map(), ow_ecs2:world()) -> ok.
%process_actors([], _ZoneData, _World) ->
%    ok;
%process_actors([{ID, Components} | Rest], ZoneData, World) ->
%    % Apply input for this actor, simulating one frame per input frame as long
%    % as it is within the valid frames period.
%    case ks_input:apply(key_table(), ZoneData, World, ID) of
%        {ok, 0} ->
%            % Nothing queued, just simulate one tick with no input
%            K = ow_ecs2:get(kinematics, Components),
%            update_position(K, ID, ZoneData, World),
%            K2 = ow_ecs2:get(kinematics, Components),
%            update_rotation(K2, ID, ZoneData, World);
%        {ok, N} -> 
%            % Already simulated, continue
%            case N > 1 of
%                false -> ok;
%                true ->
%                    logger:notice("Simulated ~p this frame", [N]),
%                    ok
%            end
%    end,
%    % Process the rest of the actors.
%    process_actors(Rest, ZoneData, World).

% For each actor, apply one input, simulate the world state, then process any
% remaining inputs.
%process_actors([], _ZoneData, _World) ->
%    ok;
%process_actors([{ID, Components} = Actor | Rest], ZoneData, World) -> 
%    case ks_input:apply(key_table(), ZoneData, World, ID) of
%        {ok, 0} ->
%            K = ow_ecs2:get(kinematics, Components),
%            update_position(K, ID, ZoneData, World),
%            K2 = ow_ecs2:get(kinematics, Components),
%            update_rotation(K2, ID, ZoneData, World),
%            process_actors(Rest, ZoneData, World);
%        {ok, _N} ->
%            K = ow_ecs2:get(kinematics, Components),
%            update_position(K, ID, ZoneData, World),
%            K2 = ow_ecs2:get(kinematics, Components),
%            update_rotation(K2, ID, ZoneData, World),
%            process_actors(Rest ++ [Actor], ZoneData, World)
%    end.
%
%-spec apply(move(), direction(), ks_actor:actor(), map(), ow_ecs2:world()) -> ok.
%apply(Type, Direction, ID, ZoneData, World) ->
%    Components = ow_ecs2:entity(ID, World),
%    Kinematics = ow_ecs2:get(kinematics, Components),
%    Torque = ow_ecs2:get(torque, Components),
%    AngularMass = ow_ecs2:get(angular_mass, Components),
%    Thrust = ow_ecs2:get(thrust, Components),
%    Mass = ow_ecs2:get(mass, Components),
%    case Type of
%        impulse ->
%            K = apply_impulse(Direction, Kinematics, Mass, Thrust),
%            update_position(K, ID, ZoneData, World);
%        rotate ->
%            K = apply_rotation(Direction, Kinematics, AngularMass, Torque),
%            update_rotation(K, ID, ZoneData, World)
%    end.
%
%-spec apply_impulse(direction(), kinematics(), integer(), integer()) -> kinematics().
%apply_impulse(Direction, Kinematics, Mass, Thrust) ->
%    % Apply an impulse in a direction applies an acceleration vector in the
%    % selected direction
%    <<LThrust:8, BThrust:8, RThrust:8, TThrust:8>> = Thrust,
%    % Get the current velocity, acceleration and rotation
%    #{acc_t := {Xa, Ya}, pos_r := Rot} = Kinematics,
%    Acc =
%        case Direction of
%            % The direction of movement is opposite the thrust direction
%            % Fwd = Bottom Thruster
%            % Rev = Top thruster
%            % Left = Right thruster
%            % Right = Left thruster
%            fwd ->
%                {0 * BThrust / Mass, 1 * BThrust / Mass};
%            rev ->
%                {0 * TThrust / Mass, -1 * TThrust / Mass};
%            left ->
%                {1 * RThrust / Mass, 0 * RThrust / Mass};
%            right ->
%                {-1 * LThrust / Mass, 0 * LThrust / Mass}
%        end,
%    % Rotate the keyed vector by the current rotation
%    {Xa2, Ya2} = ow_vector:rotate(Acc, Rot),
%    % Subtract (negative Y is up) from the original vector
%    Acc1 = {Xa - Xa2, Ya - Ya2},
%    Kinematics#{acc_t := Acc1}.
%
%update_position(Kinematics, ID, ZoneData, World) ->
%    #{tick_ms := TickMs, env := Env} = ZoneData,
%    #{max_vel_t := MaxVel, acc_factor := AccFactor} = Env,
%    #{pos_t := Pos0, vel_t := Vel0, acc_t := Acc0} = Kinematics,
%    DeltaT = TickMs / 1000,
%    % Scale acceleration by the acceleration factor
%    Acc1 = ow_vector:scale(Acc0, AccFactor),
%    % Calculate the new velocity
%    % v = v0 + a*t
%    Vel = ow_vector:add(Vel0, ow_vector:scale(Acc1, DeltaT)),
%    % Fix the velocity to the environment maximum
%    Vel1 =
%        case ow_vector:length_squared(Vel) >= math:pow(MaxVel, 2) of
%            true ->
%                ow_vector:scale(ow_vector:normalize(Vel), MaxVel);
%            false ->
%                Vel
%        end,
%    Pos1 = ow_vector:add(Pos0, ow_vector:scale(Vel1, DeltaT)),
%    % Update the velocity, position
%    % Then set acceleration back to 0 in preparation for the next frame
%    Kinematics1 = Kinematics#{
%        pos_t := Pos1,
%        vel_t := Vel1,
%        acc_t := {0, 0}
%    },
%    ow_ecs2:add_component(kinematics, Kinematics1, ID, World).
%
%-spec apply_rotation(left | right, kinematics(), float(), integer()) -> kinematics().
%apply_rotation(Direction, Kinematics, AngularMass, Torque) ->
%    #{vel_r := VelR} = Kinematics,
%    D =
%        case Direction of
%            right -> 1.0;
%            left -> -1.0
%        end,
%    VelR1 = VelR + D * (Torque / AngularMass),
%    Kinematics#{vel_r := VelR1}.
%
%-spec update_rotation(map(), integer(), term(), ow_ecs2:world()) -> ok.
%update_rotation(Kinematics, ID, ZoneData, World) ->
%    % Get the zone data
%    #{tick_ms := TickMs, env := Env} = ZoneData,
%    #{max_vel_r := MaxVelR, torque_factor := TorqueFactor} = Env,
%    % Now apply the physics 
%    #{pos_r := RotP, vel_r := RotV} = Kinematics,
%    DeltaT = TickMs / 1000,
%    Components = ow_ecs2:entity(ID, World),
%    AngularMass = ow_ecs2:get(angular_mass, Components),
%    Torque = ow_ecs2:get(torque, Components),
%    RotationIncrement = (Torque * TorqueFactor) / AngularMass,
%    PerShipMaxVelR = floor(MaxVelR / RotationIncrement) * RotationIncrement,
%    RotV2 =
%        case RotV of
%            _ when RotV < -PerShipMaxVelR ->
%                -PerShipMaxVelR;
%            _ when RotV > PerShipMaxVelR ->
%                PerShipMaxVelR;
%            _ ->
%                RotV
%        end,
%    RotP1 = RotP + RotV2 * DeltaT,
%    KinematicsUpdate = Kinematics#{pos_r := RotP1, vel_r := RotV2},
%    ow_ecs2:add_component(kinematics, KinematicsUpdate, ID, World).
%
%%-spec update_rotation([ow_ecs2:entity()], term(), ow_ecs2:world()) -> ok.
%%update_rotation([], _ZoneData, _World) ->
%%    ok;
%%update_rotation([{ID, Components} | R], ZoneData, World) ->
%%    #{tick_ms := TickMs, env := Env} = ZoneData,
%%    #{max_vel_r := MaxVelR, torque_factor := TorqueFactor} = Env,
%%    % Now apply the physics
%%    Kinematics = ow_ecs2:get(kinematics, Components),
%%    #{pos_r := RotP, vel_r := RotV} = Kinematics,
%%    DeltaT = TickMs / 1000,
%%    AngularMass = ow_ecs2:get(angular_mass, Components),
%%    Torque = ow_ecs2:get(torque, Components),
%%    RotationIncrement = (Torque * TorqueFactor) / AngularMass,
%%    PerShipMaxVelR = floor(MaxVelR / RotationIncrement) * RotationIncrement,
%%    RotV2 =
%%        case RotV of
%%            _ when RotV < -PerShipMaxVelR ->
%%                -PerShipMaxVelR;
%%            _ when RotV > PerShipMaxVelR ->
%%                PerShipMaxVelR;
%%            _ ->
%%                RotV
%%        end,
%%    RotP1 = RotP + RotV2 * DeltaT,
%%   % ToLog = io_lib:format("~.16f ~.16f ~.16f ~.16f", [RotP, RotV2, DeltaT, RotP1]),
%%   % logger:notice(ToLog),
%%    KinematicsUpdate = Kinematics#{pos_r := RotP1, vel_r := RotV2},
%%    ow_ecs2:add_component(kinematics, KinematicsUpdate, ID, World),
%%    update_rotation(R, ZoneData, World).
%%
%%-spec update_position([ow_ecs2:entity()], term(), ow_ecs2:world()) -> ok.
%%update_position([], _ZoneData, _World) ->
%%    ok;
%%update_position([{ID, Components} | R], ZoneData, World) ->
%%    #{tick_ms := TickMs, env := Env} = ZoneData,
%%    #{max_vel_t := MaxVel, acc_factor := AccFactor} = Env,
%%    % Now apply the physics
%%    Kinematics = ow_ecs2:get(kinematics, Components),
%%    #{pos_t := Pos0, vel_t := Vel0, acc_t := Acc0} = Kinematics,
%%    DeltaT = TickMs / 1000,
%%    % Scale acceleration by the acceleration factor
%%    Acc1 = ow_vector:scale(Acc0, AccFactor),
%%    % Calculate the new velocity
%%    % v = v0 + a*t
%%    Vel = ow_vector:add(Vel0, ow_vector:scale(Acc1, DeltaT)),
%%    % Fix the velocity to the environment maximum
%%    Vel1 =
%%        case ow_vector:length_squared(Vel) >= math:pow(MaxVel, 2) of
%%            true ->
%%                ow_vector:scale(ow_vector:normalize(Vel), MaxVel);
%%            false ->
%%                Vel
%%        end,
%%    Pos1 = ow_vector:add(Pos0, ow_vector:scale(Vel1, DeltaT)),
%%    % Update the velocity, position
%%    % Then set acceleration back to 0 in preparation for the next frame
%%    Kinematics1 = Kinematics#{
%%        pos_t := Pos1,
%%        vel_t := Vel1,
%%        acc_t := {0, 0}
%%    },
%%    ow_ecs2:add_component(kinematics, Kinematics1, ID, World),
%%    update_position(R, ZoneData, World).
