-module(ks_phys).

-export([proc_phys/2, apply_move/4, phys_to_tuple/1]).

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

-spec key_table() -> list().
key_table() ->
    [
        {'IMPULSE_FWD', fun(ID, _, Q) -> apply_move(impulse, fwd, ID, Q) end},
        {'IMPULSE_REV', fun(ID, _, Q) -> apply_move(impulse, rev, ID, Q) end},
        {'IMPULSE_LEFT', fun(ID, _, Q) -> apply_move(impulse, left, ID, Q) end},
        {'IMPULSE_RIGHT', fun(ID, _, Q) -> apply_move(impulse, right, ID, Q) end},
        {'ROTATE_LEFT', fun(ID, _, Q) -> apply_move(rotate, left, ID, Q) end},
        {'ROTATE_RIGHT', fun(ID, _, Q) -> apply_move(rotate, right, ID, Q) end}
    ].

-spec proc_phys(ow_ecs:query(), term()) -> ok.
proc_phys(Query, #{tick_ms := TickMs}) ->
    % Match the input and components for any changes this tick
    Actors = ow_ecs:match_components([input, phys], Query),
    % Apply the input for any player who has the input component, then zero out
    % the component.
    process_actors(Actors, Query),
    % Update positions, including non-actors
    PhysEntities = ow_ecs:match_component(phys, Query),
    update_positions(PhysEntities, TickMs, Query).

% Recurse over all of the actors that match the component
-spec process_actors([ow_ecs:entity()], ow_ecs:query()) -> ok.
process_actors([], _Query) ->
    ok;
process_actors([{ID, Components} | Rest], Query) ->
    InputList = ow_ecs:get(input, Components),
    ks_input:apply(InputList, ID, key_table(), Query),
    process_actors(Rest, Query).

-spec apply_move(move(), direction(), ks_actor:actor(), ow_ecs:query()) -> ok.
apply_move(Type, Direction, ID, Query) ->
    {ID, Components} = ow_ecs:entity(ID, Query),
    Phys = ow_ecs:get(phys, Components),
    Stats = ow_ecs:get(stats, Components),
    NewPhys =
        case Type of
            impulse ->
                apply_impulse(Direction, Phys, Stats);
            rotate ->
                apply_rotation(Direction, Phys, Stats)
        end,
    ow_ecs:add_component(phys, NewPhys, ID, Query).

-spec apply_impulse(direction(), phys(), map()) -> phys().
apply_impulse(Direction, Phys, Stats) ->
    #{speed_fac := Speed, max_vel := MaxV} = Stats,
    % Get the current velocity
    {_Pos, {Xv, Yv}, Rot} = phys_to_tuple(Phys),
    % 1 keypress = Vector2(0,-1) * Speed
    Vel1 =
        case Direction of
            fwd ->
                {0 * Speed, 1 * Speed};
            rev ->
                {0 * Speed, -1 * Speed};
            left ->
                {-1 * Speed, 0 * Speed};
            right ->
                {1 * Speed, 0 * Speed}
        end,
    % Rotate the keyed vector by the current rotation
    {Xv2, Yv2} = ow_vector:rotate(Vel1, Rot),
    % Subtract (negative Y is up) from the original vector
    Vel3 = {Xv - Xv2, Yv - Yv2},
    % Check to see if the velocity exceeds max velocity
    Vel4 =
        case ow_vector:length_squared(Vel3) > math:pow(MaxV, 2) of
            true ->
                ow_vector:scale(ow_vector:normalize(Vel3), MaxV);
            false ->
                Vel3
        end,
    % Update the physics component
    Phys#{vel => ow_vector:vector_map(Vel4)}.

-spec apply_rotation(left | right, phys(), map()) -> phys().
apply_rotation(Direction, Phys, Stats) ->
    {_Pos, _Vel, Rot} = phys_to_tuple(Phys),
    #{rotation_fac := RFactor} = Stats,
    D =
        case Direction of
            right -> 1.0;
            left -> -1.0
        end,
    Rot1 = Rot + (D / RFactor),
    Phys#{rot => Rot1}.

-spec phys_to_tuple(map()) -> {vector(), vector(), integer()}.
phys_to_tuple(#{pos := #{x := Xp, y := Yp}, vel := #{x := Xv, y := Yv}, rot := Rot}) ->
    Pos = {Xp, Yp},
    Vel = {Xv, Yv},
    {Pos, Vel, Rot}.

-spec update_positions([ow_ecs:entity()], term(), ow_ecs:query()) -> ok.
update_positions([], _TickMs, _Query) ->
    ok;
update_positions([{ID, Components} | Rest], TickMs, Query) ->
    Phys = ow_ecs:get(phys, Components),
    #{pos := PosMap, vel := VelMap} = Phys,
    #{x := Xv, y := Yv} = VelMap,
    #{x := Xp, y := Yp} = PosMap,
    DeltaT = TickMs / 1000,
    NewPos = {
        Xp + (Xv * DeltaT),
        Yp + (Yv * DeltaT)
    },
    Phys1 = Phys#{pos => ow_vector:vector_map(NewPos)},
    ow_ecs:add_component(phys, Phys1, ID, Query),
    update_positions(Rest, TickMs, Query).
