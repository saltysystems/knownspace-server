-module(ks_randomwalk).
-export([walk/2, walk/3]).

%% takes a initial value

walk(Steps, InitialVal, StepSize) ->
    walk(Steps, InitialVal, StepSize, []).

walk(Steps, InitialVal) ->
    %% default to a step size of 1
    walk(Steps, InitialVal, 1, []).

walk(Steps, _, _, Acc) when Steps < 1 ->
    Acc;
walk(Steps, InitialVal, StepSize, Acc) ->
    case rand:uniform(2) of
        1 -> NewVal = InitialVal - StepSize;
        2 -> NewVal = InitialVal + StepSize
    end,
    walk(Steps - 1, NewVal, StepSize, [NewVal | Acc]).
