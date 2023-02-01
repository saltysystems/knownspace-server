-module(ks_reactor).
% Ship's reactor. Energy is drained every time a fire is shot, then recharges.

-export([new/0, proc_reactor/2]).

new() ->
    #{
        now => 0,
        max => 0,
        rate => 0
    }.

proc_reactor(World, _ZoneData) ->
    % Match any entities who have input this tick
    Fun =
        fun(ID, Reactor) ->
            #{
                now := Cur,
                max := Max,
                rate := R
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
                    ow_ecs:add_component(reactor, Reactor#{now => Cur1}, ID, World);
                true ->
                    ok
            end
        end,
    ow_ecs:foreach_component(Fun, reactor, World).
