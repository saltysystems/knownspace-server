-module(ks_input).

-export([apply/4, push/3, proc_reset/1]).

% Recurse over all of the inputs buffered this tick.
% One optimization may be to do all of the manipulations here and pass the
% entity data forward until the final entity update is complete, _then_
% persist to ETS. Benchmark.
-spec apply(list(), integer(), list(), ow_ecs:world()) -> ok.
apply([], _ID, _KeyList, _World) ->
    ok;
apply([Input | Rest], ID, Actions, World) ->
    Cursor = maps:get(cursor, Input, undefined),
    KeyList = maps:get(keys, Input, undefined),
    Apply =
        fun(KeyPress) ->
            case lists:keyfind(KeyPress, 1, Actions) of
                {KeyPress, Fun} ->
                    Fun(ID, Cursor, World);
                false ->
                    % maybe an input not for us
                    ok
            end
        end,
    lists:foreach(Apply, KeyList),
    %% Apply the remaining inputs for this actor
    apply(Rest, ID, Actions, World).

push(Input1, ID, World) ->
    logger:debug("Adding input component to ~p with data ~p", [ID, Input1]),
    % If there's already input, we need to merge it.
    case ow_ecs:try_component(input, ID, World) of
        false ->
            % Just add it
            ow_ecs:add_component(input, [Input1], ID, World);
        Components ->
            Input0 = ow_ecs:get(input, Components),
            Input = [Input1 | Input0],
            ow_ecs:add_component(input, Input, ID, World)
    end.

proc_reset(World) ->
    % Get all entities with input
    E = ow_ecs:match_component(input, World),
    [ow_ecs:del_component(input, ID, World) || {ID, _} <- E].
