-module(ks_projectile).

-export([key_table/0, proc_projectile/1, notify/1]).

% These will need to be based on database or ETS lookups, ultimately. Or
% derived based on a mix.
-define(TTL, 5000).
-define(SPEED, 600).
-define(HITBOX, [
    {-2, -2},
    {-2, 2},
    {2, 2},
    {2, -2}
]).
-define(SHOOT_POWER, 35).

key_table() ->
    % This table is the master list of all input functions along with the
    % associated function that should be applied when pressed
    [
        {'ACTION_0', fun(ID, C, Q) -> shoot_if_powered(ID, C, Q) end}
    ].

proc_projectile(World) ->
    % Update the TTL for any projectiles
    update_and_expire(World),
    % Check if any actor created a projectile this tick
    new_projectiles_from_input(World),
    % Delete any projectiles that have the collision key
    delete_collided(World).

notify(World) ->
    % Match the list of projectiles that are new in the last tick
    Projectiles = ow_ecs2:match_components([projectile, ttl], World),
    Fun =
        fun(E = {_ID, Components}, AccIn) ->
            TTL = ow_ecs2:get(ttl, Components),
            case TTL of
                ?TTL ->
                    % New projectile made this tick
                    [ow_ecs2:to_map(E) | AccIn];
                _Other ->
                    AccIn
            end
        end,
    lists:foldl(Fun, [], Projectiles).

new_projectiles_from_input(World) ->
    % Match the list of actors that have input this tick and see if any ask to
    % create new projectiles
    Actors = ow_ecs2:match_components([input, reactor], World),
    Fun = fun({ID, Components}) ->
        InputList = ow_ecs2:get(input, Components),
        ks_input:apply(InputList, ID, key_table(), World)
    end,
    lists:foreach(Fun, Actors).

shoot_if_powered(Owner, Cursor, World) ->
    Components = ow_ecs2:entity(Owner, World),
    ReactorMap = ow_ecs2:get(reactor, Components),
    Charge = maps:get(now, ReactorMap),
    if
        Charge >= ?SHOOT_POWER ->
            % We have enough juice to shoot. Fire!
            create_projectile(Owner, Cursor, World),
            % Subtract juice from the reactor
            NewCharge = Charge - ?SHOOT_POWER,
            ow_ecs2:add_component(
                reactor,
                ReactorMap#{now => NewCharge},
                Owner,
                World
            );
        true ->
            % Not enough juice. Don't fire.
            ok
    end.

create_projectile(Owner, Cursor, World) ->
    Components = ow_ecs2:entity(Owner, World),
    Phys = ow_ecs2:get(phys, Components),
    % Match the subcomponents that have some sort of weapon attached
    %B = ks_shipgrid:match_subcomponents(action, 
    % Draw a line between the cursor position and the actor's current position
    % to get a direction vector.
    #{x := Xc, y := Yc} = Cursor,
    % Snap the point to the arc.. TODO-FEATURE
    % https://stackoverflow.com/questions/6270785
    {Pos = {Xe, Ye}, _Vel, _Rot} = ks_phys:phys_to_tuple(Phys),
    % Normalize the vector, ensure its nonzero, and scale it.
    Speed = ?SPEED,
    Direction = {Xc - Xe, Yc - Ye},
    % test the length of the direction. if it's ~0 , then make it non-zero so
    % it can be normalized
    SanitizedDir =
        case Direction of
            {0.0, 0.0} -> {0.1, 0.1};
            {0, 0} -> {1, 1};
            _ -> Direction
        end,
    Vel = ow_vector:scale(ow_vector:normalize(SanitizedDir), Speed),
    ID = erlang:unique_integer(),
    ProjPhys =
        #{
            pos => ow_vector:vector_map(Pos),
            vel => ow_vector:vector_map(Vel),
            rot => 0
        },
    Type = ks_pb:enum_value_by_symbol(action_type, 'BALLISTIC'),
    Hitbox = ow_vector:rect_to_maps(?HITBOX),
    % ms
    TTL = ?TTL,
    CreateTime = erlang:system_time(),
    ow_ecs2:add_component(projectile, true, ID, World),
    ow_ecs2:add_component(type, Type, ID, World),
    ow_ecs2:add_component(owner, Owner, ID, World),
    ow_ecs2:add_component(phys, ProjPhys, ID, World),
    ow_ecs2:add_component(hitbox, Hitbox, ID, World),
    ow_ecs2:add_component(ttl, TTL, ID, World),
    ow_ecs2:add_component(create_time, CreateTime, ID, World).

update_and_expire(World) ->
    Projectiles = ow_ecs2:match_component(ttl, World),
    Fun =
        fun({ID, Components}) ->
            Remaining = ow_ecs2:get(ttl, Components),
            case Remaining =< 0 of
                true ->
                    % Delete the entity
                    ow_ecs2:rm_entity(ID, World);
                false ->
                    % Tick down
                    Now = erlang:system_time(),
                    CreateTime = ow_ecs2:get(create_time, Components),
                    Delta = Now - CreateTime,
                    T = erlang:convert_time_unit(Delta, native, millisecond),
                    ow_ecs2:add_component(ttl, ?TTL - T, ID, World)
            end
        end,
    lists:foreach(Fun, Projectiles).

delete_collided(World) ->
    Projectiles = ow_ecs2:match_component(collision, World),
    Fun =
        fun({ID, _Components}) ->
            ow_ecs2:rm_entity(ID, World)
        end,
    lists:foreach(Fun, Projectiles).
