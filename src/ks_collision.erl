-module(ks_collision).

-export([proc_collision/2, apply_collisions/3, notify/1]).

notify(World) ->
    % Lookup all projectile entities that have collisions this tick
    Projectiles = ow_ecs2:match_components([projectile, collision], World),
    Fun = fun({ProjID, Keys}, Acc) ->
        {collision, ActorID} = lists:keyfind(collision, 1, Keys),
        [#{id => [ProjID, ActorID]} | Acc]
    end,
    Collisions = lists:foldl(Fun, [], Projectiles),
    %case Collisions of
    %    [] -> [];
    %    C -> logger:notice("Collision at: ~p~n", [C])
    %end,
    Collisions.

proc_collision(#{boundary := Boundary}, World) ->
    % Match actors with physics and a hitbox
    Actors = ow_ecs2:match_components([phys, hull, actor], World),
    % Match projectiles with physics and a hitbox
    Projectiles = ow_ecs2:match_components([phys, hitbox, projectile], World),
    % Possibly can simplify this by jus passing the {ID, [Keys]} format to
    % apply collisions and selecting appropriately.
    % Calculate collisions and update the Projectile.
    Collisions = apply_collisions(Actors, Projectiles, Boundary),
    % TODO: Once the collision has proc'd - need to handle the damage.
    [update_projectile(O1, O2, World) || {O1, O2} <- Collisions].

apply_collisions(_, [], _Boundary)->
    [];
apply_collisions(Actors, Projectiles, QuadTree) ->
    lists:flatten([area_entered(P, Actors, QuadTree) || P <- Projectiles]).
    % Filter out the duplicate pairs
    %ow_util:remove_dups(Collisions).

area_entered(Projectile, Actors, QuadTree) ->
    {_ProjID, PComponents} = Projectile,
    Owner = ow_ecs2:get(owner, PComponents),
    % Delete the parent  from the possible hit list
    Actors2 = lists:keydelete(Owner, 1, Actors),
    HitboxFun =
        fun({_ID, Components}) ->
            Phys = ow_ecs2:get(phys, Components),
            #{pos := PosMap, rot := Rot} = Phys,
            Pos = ow_vector:vector_tuple(PosMap),
            Hull = 
            case ow_ecs2:get(hull, Components) of 
                false ->
                    % Try to check the hitbox instead
                    HB = ow_ecs2:get(hitbox, Components),
                    ow_vector:rect_to_tuples(HB);
                H -> H
            end,
            % Rotate the hitbox by the rotation
            HullRotate = ow_vector:rotate_polygon(Hull, Rot),
            % Translate the hitbox into place
            ow_vector:translate(HullRotate, Pos)
            % Convert the bounding box to a list of tuples
            %ow_vector:rect_to_tuples(HitboxTranslate)
        end,
    PosFun =
        fun({_ID, Components}) ->
            Phys = ow_ecs2:get(phys, Components),
            #{pos := Pos} = Phys,
            ow_vector:vector_tuple(Pos)
        end,
    QuadTree2 = ow_collision:add_entities([Projectile | Actors2], PosFun, QuadTree),
    % For every entity, check an area a bit beyond its position for potential
    % collisions. TODO: Does this need to be a function of tickrate, latency,
    % etc ?
    {X, Y} = get_position(Projectile),
    % Check a significant potential area around the entity. This is NOT the
    % collision detection, just the POTENTIAL FOR collisions. It's not clear
    % how to set these values to be reasonable for objects of dynamic size.
    Left = X - 25,
    Bottom = Y - 25,
    Right = X + 25,
    Top = Y + 25,
    Results = ow_collision:check_area(
        {Left, Bottom, Right, Top}, HitboxFun, QuadTree2
    ),
    [{O1, O2} || {O1, O2, Coll} <- Results, Coll == true].

update_projectile(O1 = #{actor := true}, O2 = #{projectile := true}, World) ->
    % Swap argument order if the Projectile is the 2nd argument
    update_projectile(O2, O1, World);
update_projectile(
    #{projectile := true, id := ProjectileID, owner := OwnerID},
    #{actor := true, id := ActorID},
    World
) when OwnerID =/= ActorID ->
    ow_ecs2:add_component(collision, ActorID, ProjectileID, World);
update_projectile(_, _, _Q) ->
    % Ignore anything else
    ok.

get_position({_ID, Components}) ->
    Phys = ow_ecs2:get(phys, Components),
    #{pos := Pos} = Phys,
    ow_vector:vector_tuple(Pos).
