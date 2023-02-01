-module(ks_phys).

-export([proc_phys/2, apply/5, phys_to_tuple/1]).

-type direction() :: fwd | rev | left | right.
-type move() :: impulse | rotate.
-type vector() :: ow_vector:vector().
-type vector_map() :: #{
    x => number(),
    y => number()
}.
-type phys() :: #{
    pos => vector_map(),
    vel => vector_map(),
    rot => number()
}.

-spec key_table(map()) -> list().
key_table(ZD) ->
    [
        {'IMPULSE_FWD', fun(ID, _, W) -> apply(impulse, fwd, ZD, ID, W) end},
        {'IMPULSE_REV', fun(ID, _, W) -> apply(impulse, rev, ZD, ID, W) end},
        {'IMPULSE_LEFT', fun(ID, _, W) -> apply(impulse, left, ZD, ID, W) end},
        {'IMPULSE_RIGHT', fun(ID, _, W) -> apply(impulse, right, ZD, ID, W) end},
        {'ROTATE_LEFT', fun(ID, _, W) -> apply(rotate, left, ZD, ID, W) end},
        {'ROTATE_RIGHT', fun(ID, _, W) -> apply(rotate, right, ZD, ID, W) end}
    ].

-spec proc_phys(ow_ecs:world(), term()) -> ok.
proc_phys(World, ZoneData) -> 
    % Match the input and components for any changes this tick
    Actors = ow_ecs:match_components([input, phys], World),
    % Apply the input for any player who has the input component, then zero out
    % the component.
    process_actors(Actors, ZoneData, World),
    % Update positions, including non-actors
    PhysEntities = ow_ecs:match_component(phys, World),
    update_positions(PhysEntities, ZoneData, World).

% Recurse over all of the actors that match the component
-spec process_actors([ow_ecs:entity()], map(), ow_ecs:world()) -> ok.
process_actors([], _ZoneData, _World) ->
    ok;
process_actors([{ID, Components} | Rest], ZoneData, World) ->
    InputList = ow_ecs:get(input, Components),
    ks_input:apply(InputList, ID, key_table(ZoneData), World),
    process_actors(Rest, ZoneData, World).

-spec apply(move(), direction(), map(), ks_actor:actor(), ow_ecs:world()) -> ok.
apply(Type, Direction, ZoneData, ID, World) ->
    Components = ow_ecs:entity(ID, World),
    Phys = ow_ecs:get(phys, Components),
    Torque = ow_ecs:get(torque, Components),
    AngularMass = ow_ecs:get(angular_mass, Components),
    Thrust = ow_ecs:get(thrust, Components),
    NewPhys =
        case Type of
            impulse ->
                MaxVel = ks_zone2:get_env(max_vel, ZoneData),
                apply_impulse(Direction, Phys, Thrust, MaxVel);
            rotate ->
                apply_rotation(Direction, Phys, AngularMass, Torque)
        end,
    ow_ecs:add_component(phys, NewPhys, ID, World).

-spec apply_impulse(direction(), phys(), integer(), integer()) -> phys().
apply_impulse(Direction, Phys, Thrust, MaxVel) ->
    % Apply an impulse in a direction increases or decreases the velocity
    % vector by a fixed amount.
    <<LThrust:8, BThrust:8, RThrust:8, TThrust:8>> = Thrust,
    % Get the current velocity
    {_Pos, {Xv, Yv}, Rot} = phys_to_tuple(Phys),
    % 1 keypress = Vector2(0,-1) * Speed
    Vel1 =
        case Direction of
            % The direction of movement is opposite the thrust direction
            % Fwd = Bottom Thruster
            % Rev = Top thruster
            % Left = Right thruster
            % Right = Left thruster
            fwd ->
                {0 * BThrust, 1 * BThrust};
            rev ->
                {0 * TThrust, -1 * TThrust};
            left ->
                {-1 * RThrust, 0 * RThrust};
            right ->
                {1 * LThrust, 0 * LThrust}
        end,
    % Rotate the keyed vector by the current rotation
    {Xv2, Yv2} = ow_vector:rotate(Vel1, Rot),
    % Subtract (negative Y is up) from the original vector
    Vel3 = {Xv - Xv2, Yv - Yv2},
    % Cap the max velocity based on the zone speed limit
    Vel4 =
        case ow_vector:length_squared(Vel3) > math:pow(MaxVel, 2) of
            true ->
                ow_vector:scale(ow_vector:normalize(Vel3), MaxVel);
            false ->
                Vel3
        end,
    % Update the physics component
    Phys#{vel => ow_vector:vector_map(Vel4)}.

-spec apply_rotation(left | right, phys(), float(), integer()) -> phys().
apply_rotation(Direction, Phys, AngularMass, Torque) ->
    % For now, we ignore angular mass and fake this calculation. 
    % TODO-FEATURE: Add some mass into the equation, calculate the rotational
    % position (currently called rot) and the rotational velocity
    {_Pos, _Vel, Rot} = phys_to_tuple(Phys),
    D =
        case Direction of
            right -> 1.0;
            left -> -1.0
        end,
    % should be:
    % ra = T/I 
    % rv = ra * DeltaT 
    % But that's not what we're calculating here because we want to know the
    % exact rotation. This really should be the rotational velocity we're
    % calculating but we fudge it a bit :)
    Rot1 = Rot + D * (Torque / AngularMass),
    Phys#{rot => Rot1}.

-spec update_positions([ow_ecs:entity()], term(), ow_ecs:world()) -> ok.
update_positions([], _ZoneData, _World) ->
    ok;
update_positions([{ID, Components} | R], ZD = #{ tick_ms := TickMs }, World) ->
    % Now apply the physics 
    Phys = ow_ecs:get(phys, Components),
    {{Xp,Yp}, {Xv, Yv}, _Rot} = phys_to_tuple(Phys),
    DeltaT = TickMs / 1000,
    NewPos = {
        Xp + (Xv * DeltaT),
        Yp + (Yv * DeltaT)
    },
    Phys1 = Phys#{pos => ow_vector:vector_map(NewPos)},
    ow_ecs:add_component(phys, Phys1, ID, World),
    %io:format("POS: ~p; VEL: ~p; ROT: ~p~n", [NewPos, {Xv,Yv}, Rot]),
    update_positions(R, ZD, World).

-spec phys_to_tuple(map()) -> {vector(), vector(), integer()}.
phys_to_tuple(#{pos := #{x := Xp, y := Yp}, vel := #{x := Xv, y := Yv}, rot := Rot}) ->
    Pos = {Xp, Yp},
    Vel = {Xv, Yv},
    {Pos, Vel, Rot}.
