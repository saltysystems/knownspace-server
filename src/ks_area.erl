-module(ks_area).

-export([search/4]).

search({X, Y}, Range, QuadTree, World) ->
    % Match actors with physics and a hitbox
    Actors = ow_ecs2:match_components([kinematics, hull, actor], World),
    PosFun =
        fun({_ID, Components}) ->
            Phys = ow_ecs2:get(kinematics, Components),
            #{pos_t := Pos} = Phys,
            Pos
        end,
    QuadTree2 = ow_collision:add_entities(Actors, PosFun, QuadTree),
    erlquad:area_query(X - Range, Y - Range, X + Range, Y + Range, QuadTree2).
