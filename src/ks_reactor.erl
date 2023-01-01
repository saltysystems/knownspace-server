-module(ks_reactor).
% Ship's reactor. Energy is drained every time a fire is shot, then recharges.

-export([new/0, proc_reactor/2]).

new() ->
    #{
        cur_reactor => 0,
        max_reactor => 0,
        rate_reactor => 0
    }.

proc_reactor(Query, _ZoneData) ->
    % Match any entities who have input this tick
    Fun =
        fun(ID, Reactor) ->
            #{
                cur_reactor := Cur,
                max_reactor := Max,
                rate_reactor := R
            } = Reactor,
            Cur1 =
                if
                    Cur < Max ->
                        % Recharge the reactor
                        Cur + R;
                    true ->
                        % Set the reactor to max
                        Max
                end,
            if
                Cur =/= Max ->
                    % Changed, so update the table
                    ow_ecs:add_component(reactor, Reactor#{cur_reactor => Cur1}, ID, Query);
                true ->
                    ok
            end
        end,
    ow_ecs:foreach_component(Fun, reactor, Query).
