-module(ks_reactor).
% Ship's reactor. Energy is drained every time a fire is shot, then recharges.

-export([proc_reactor/2]).

proc_reactor(Query, _ZoneData) ->
    % Match any entities who have input this tick
    Fun =
        fun(ID, ComponentValue) ->
            {Cur, Max, R} = ComponentValue,
            logger:notice("Reactor charge: ~p", [Cur]),
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
                    ow_ecs:add_component(reactor, {Cur1, Max, R}, ID, Query);
                true ->
                    ok
            end
        end,
    ow_ecs:foreach_component(Fun, reactor, Query).
