-module(ks_projectile).

-export([key_table/0, proc_projectile/1, notify/1]).

% These will need to be based on database or ETS lookups, ultimately. Or
% derived based on a mix.
-define(TTL, 5000).
-define(SPEED, 300).
-define(HITBOX, [
    {-10, -10},
    {-10, 10},
    {10, -10},
    {10, 10}
]).
-define(SHOOT_POWER, 35).

key_table() ->
    % This table is the master list of all input functions along with the
    % associated function that should be applied when pressed
    [
        {'ACTION_0', fun(ID, C, Q) -> maybe_shoot(ID, C, Q) end}
    ].

proc_projectile(Query) ->
    % Update the TTL for any projectiles
    update_and_expire(Query),
    % Check if any actor created a projectile this tick
    new_projectiles_from_input(Query),
    % Delete any projectiles that have the collision key
    delete_collided(Query).

notify(World) ->
    % Match the list of projectiles that are new in the last tick
    Query = ow_ecs:query(World),
    Projectiles = ow_ecs:match_components([projectile, ttl], Query),
    Fun =
        fun(E = {_ID, Components}, AccIn) ->
            TTL = ow_ecs:get(ttl, Components),
            case TTL of
                ?TTL ->
                    % New projectile made this tick
                    [ow_ecs:to_map(E) | AccIn];
                _Other ->
                    AccIn
            end
        end,
    lists:foldl(Fun, [], Projectiles).

new_projectiles_from_input(Query) ->
    % Match the list of actors that have input this tick and see if any ask to
    % create new projectiles
    Actors = ow_ecs:match_components([input, reactor], Query),
    Fun = fun({ID, Components}) ->
        InputList = ow_ecs:get(input, Components),
        ks_input:apply(InputList, ID, key_table(), Query)
    end,
    lists:foreach(Fun, Actors).

maybe_shoot(Owner, Cursor, Query) ->
    {Owner, Components} = ow_ecs:entity(Owner, Query),
    ReactorMap = ow_ecs:get(reactor, Components),
    Charge = maps:get(cur_reactor, ReactorMap),
    if
        Charge >= ?SHOOT_POWER ->
            % We have enough juice to shoot. Fire!
            create_projectile(Owner, Cursor, Query),
            % Subtract juice from the reactor
            NewCharge = Charge - ?SHOOT_POWER,
            logger:notice("New Charge is: ~p", [NewCharge]),
            ow_ecs:add_component(
                reactor,
                ReactorMap#{cur_reactor => NewCharge},
                Owner,
                Query
            );
        true ->
            % Not enough juice. Don't fire.
            ok
    end.

create_projectile(Owner, Cursor, Query) ->
    {Owner, Components} = ow_ecs:entity(Owner, Query),
    Phys = ow_ecs:get(phys, Components),
    % Draw a line between the cursor position and the actor's current position
    % to get a direction vector.
    #{x := Xc, y := Yc} = Cursor,
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
    Type = ks_pb:enum_value_by_symbol(projectile_type, 'BALLISTIC'),
    Hitbox = ow_vector:rect_to_maps(?HITBOX),
    % ms
    TTL = ?TTL,
    CreateTime = erlang:system_time(),
    ow_ecs:add_component(projectile, true, ID, Query),
    ow_ecs:add_component(type, Type, ID, Query),
    ow_ecs:add_component(owner, Owner, ID, Query),
    ow_ecs:add_component(phys, ProjPhys, ID, Query),
    ow_ecs:add_component(hitbox, Hitbox, ID, Query),
    ow_ecs:add_component(ttl, TTL, ID, Query),
    ow_ecs:add_component(create_time, CreateTime, ID, Query).

update_and_expire(Query) ->
    Projectiles = ow_ecs:match_component(ttl, Query),
    Fun =
        fun({ID, Components}) ->
            Remaining = ow_ecs:get(ttl, Components),
            case Remaining =< 0 of
                true ->
                    % Delete the entity
                    ow_ecs:rm_entity(ID, Query);
                false ->
                    % Tick down
                    Now = erlang:system_time(),
                    CreateTime = ow_ecs:get(create_time, Components),
                    Delta = Now - CreateTime,
                    T = erlang:convert_time_unit(Delta, native, millisecond),
                    ow_ecs:add_component(ttl, ?TTL - T, ID, Query)
            end
        end,
    lists:foreach(Fun, Projectiles).

delete_collided(Query) ->
    Projectiles = ow_ecs:match_component(collision, Query),
    Fun =
        fun({ID, _Components}) ->
            ow_ecs:rm_entity(ID, Query)
        end,
    lists:foreach(Fun, Projectiles).
