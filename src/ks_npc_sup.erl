-module(ks_npc_sup).
-behaviour(supervisor).

-export([start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Qty) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Qty]).

init([Qty]) ->
    SupFlags = #{
        strategy => simple_one_for_one
    },
    logger:notice("Would like to spawn ~p agents", [Qty]),
    ChildSpecs = [
        #{
            id => "ks_npc",
            start => {ks_npc, start, []}
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
