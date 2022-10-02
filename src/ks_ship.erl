-module(ks_ship).

-export([new/1, decrease_hp/2, stats_to_map/1, get_cur_hp/1]).

-record(ship, {
    cur_hp :: non_neg_integer(),
    max_hp :: non_neg_integer()
}).
-type ship() :: #ship{}.

-spec new(pos_integer()) -> ship().
new(HP) ->
    #ship{cur_hp = HP, max_hp = HP}.

-spec decrease_hp(integer(), ship()) -> ship().
decrease_hp(Amount, Ship1 = #ship{cur_hp = HP}) ->
    NewHP = Ship1#ship.cur_hp - Amount,
    case NewHP < 0 of
        true ->
            Ship1#ship{cur_hp = 0};
        _ ->
            Ship1#ship{cur_hp = HP - Amount}
    end.

-spec stats_to_map(ship()) -> map().
stats_to_map(Ship) ->
    #{
        cur_hp => Ship#ship.cur_hp,
        max_hp => Ship#ship.max_hp
    }.

-spec get_cur_hp(ship()) -> non_neg_integer().
get_cur_hp(Ship) ->
    Ship#ship.cur_hp.
