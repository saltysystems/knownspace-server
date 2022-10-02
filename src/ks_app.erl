%%%-------------------------------------------------------------------
%% @doc ks public API
%% @end
%%%-------------------------------------------------------------------

-module(ks_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ow_protocol:register_app(ks),
    ks_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
