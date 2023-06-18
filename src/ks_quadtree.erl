-module(ks_quadtree).
-behaviour(gen_server).

-export([start_link/2, get_tree/0]).

% This genserver just holds the quad tree.
% This is mostly a hack to suppress really long output since the state isn't
% mutable here.

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

start_link(Boundary, TreeDepth) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Boundary, TreeDepth], []).

get_tree() ->
    gen_server:call(?MODULE, get_tree).

init([Boundary, TreeDepth]) ->
    {ok, ow_collision:new(Boundary, TreeDepth)}.

handle_call(get_tree, _From, Tree) ->
    {reply, Tree, Tree}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
