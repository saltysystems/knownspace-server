-module(ks_input).

-export([apply/4, push/3, proc_reset/1, proc_debug/2]).

% Recurse over all of the inputs buffered this tick.
% One optimization may be to do all of the manipulations here and pass the
% entity data forward until the final entity update is complete, _then_
% persist to ETS. Benchmark.
-spec apply(list(), integer(), list(), ow_ecs2:world()) -> ok.
apply([], _ID, _KeyList, _World) ->
    ok;
apply([Input | Rest], ID, Actions, World) ->
    Cursor = maps:get(cursor, Input, undefined),
    KeyList = maps:get(keys, Input, undefined),
    % Delete duplicate keys, preseve the order.
    KeyList1 = lists:uniq(KeyList),
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
    lists:foreach(Apply, KeyList1),
    %% Apply the remaining inputs for this actor
    apply(Rest, ID, Actions, World).

push(Input1, ID, World) ->
    % If there's already input, we need to merge it.
    case ow_ecs2:try_component(input, ID, World) of
        false ->
            % Just add it
            ow_ecs2:add_component(input, [Input1], ID, World);
        Components ->
            Input0 = ow_ecs2:get(input, Components),
            Input = [Input1 | Input0],
            ow_ecs2:add_component(input, Input, ID, World)
    end,
    % Log input for debugging
    log_input(Input1, ID, World).

log_input(Input, ID, World) ->
    Map = 
        case ow_ecs2:try_component(log_input, ID, World) of
            false -> #{};
            Components -> 
                ow_ecs2:get(log_input, Components)
        end,
    % Classify the input
    % Input can be a list of keys
    #{keys := Keys} = Input,
    % Fold over the list of keys this frame
    F = fun(KeyType, MapIn) ->
            Previous = maps:get(KeyType, MapIn, 0),
            MapIn#{ KeyType => Previous+1 }
        end,
    Map1 = lists:foldl(F, Map, Keys),
    ow_ecs2:add_component(log_input, Map1, ID, World).

proc_reset(World) ->
    % Get all entities with input
    E = ow_ecs2:match_component(input, World),
    [ow_ecs2:del_component(input, ID, World) || {ID, _} <- E].

proc_debug(#{frame := Frame }, World) ->
    if 
        Frame rem 100 == 0 -> 
            E = ow_ecs2:match_component(log_input, World),
            F = 
                fun({_ID, Components}) ->
                    Log = ow_ecs2:get(log_input, Components),
                    logger:notice("Input stats: ~p~n", [Log])
                end,
            [ F(Match) || Match <- E ];
        true -> ok
    end.
