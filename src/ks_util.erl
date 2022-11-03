-module(ks_util).

-export([mapfind/3,mapstore/4,mapdelete/3]).
% some borrowed map equivalents for lists:key*
% https://github.com/biokoda/bkdcore/blob/master/src/butil.erl

mapfind(V,K,[H|L]) ->
    case maps:get(K,H) of
        V ->
            H;
        _ ->
            mapfind(V,K,L)
    end;
mapfind(_,_,[]) ->
    false.

mapstore(V,K,[H|L],Map) ->
    case maps:get(K,H) of
        V ->
            [Map|L];
        _ ->
            [H|mapstore(V,K,L,Map)]
    end;
mapstore(_,_,[],Map) ->
    [Map].

mapdelete(V, K, [H|L]) ->
    case maps:get(K,H) of
        V ->
            L;
        _ ->
            mapdelete(V,K,L)
    end.
