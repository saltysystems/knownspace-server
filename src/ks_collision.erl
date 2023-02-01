-module(ks_collision).

-export([proc_collision/2, apply_collisions/2, notify/1]).

notify(World) ->
    % Lookup all projectile entities that have collisions this tick
    Projectiles = ow_ecs:match_components([projectile, collision], World),
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

proc_collision(World, #{boundary := Boundary}) ->
    % Match the phys components
    Entities = ow_ecs:match_components([phys, hitbox], World),
    % Possibly can simplify this by jus passing the {ID, [Keys]} format to
    % apply collisions and selecting appropriately.
    EntityMap = [ow_ecs:to_map(E) || E <- Entities],
    % Calculate collisions and update the Projectile.
    Collisions = apply_collisions(Boundary, EntityMap),
    [update_projectile(O1, O2, World) || {O1, O2} <- Collisions].

apply_collisions(_B, []) ->
    [];
apply_collisions(QuadTree, Entities) ->
    % Set up functions for getting the relevant position information
    PosFun =
        fun(E) ->
            #{phys := #{pos := Pos}} = E,
            ow_vector:vector_tuple(Pos)
        end,
    % Add the entities and objects to the quadtree
    C1 = ow_collision:add_entities(Entities, PosFun, QuadTree),
    % Get bounding box for entities or projectiles
    BBoxFun =
        fun(O) ->
            #{phys := #{pos := Pos, rot := _Rot}, hitbox := Hitbox} = O,
            % Rotate the hitbox by the rotation
            % TODO: is this needed? if we don't rotate it, are we gettng an
            % implicit AABB? Does it hold for multi-component ships?
            %HitboxRotate = ow_vector:rotate_polygon(Hitbox, Rot),
            % Translate the hitbox into place
            HitboxTranslate = ow_vector:translate(Hitbox, Pos),
            % Convert the bounding box to a list of tuples
            ow_vector:rect_to_tuples(HitboxTranslate)
        end,
    Collisions = lists:flatten([area_entered(O, BBoxFun, C1) || O <- Entities]),
    % Filter out the duplicate pairs
    ow_util:remove_dups(Collisions).

area_entered(Object, BoundingBoxFun, QuadTree) ->
    % For every entity, check an area a bit beyond its position for potential
    % collisions. TODO: Does this need to be a function of tickrate, latency,
    % etc ?
    {X, Y} = get_position(Object),
    % Check a significant potential area around the entity. This is NOT the
    % collision detection, just the POTENTIAL FOR collisions. It's not clear
    % how to set these values to be reasonable for objects of dynamic size.
    Left = X - 25,
    Bottom = Y - 25,
    Right = X + 25,
    Top = Y + 25,
    Results = ow_collision:check_area(
        {Left, Bottom, Right, Top}, BoundingBoxFun, QuadTree
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
    ow_ecs:add_component(collision, ActorID, ProjectileID, World);
update_projectile(_, _, _Q) ->
    % Ignore anything else
    ok.

get_position(Object) ->
    #{phys := #{pos := Pos}} = Object,
    ow_vector:vector_tuple(Pos).
