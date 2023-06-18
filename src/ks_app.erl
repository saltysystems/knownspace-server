%%%-------------------------------------------------------------------
%% @doc ks public API
%% @end
%%%-------------------------------------------------------------------

-module(ks_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ow_protocol:register(
        #{ 
            app => ks,
            router => ks_msg,
            modules => [ ks_zone2 ]
         }
    ),
    ks_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
