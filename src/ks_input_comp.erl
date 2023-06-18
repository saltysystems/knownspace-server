-module(ks_input_comp).
-behaviour(gen_server).

-export([start_link/0,
         rm_player/1,
         pop/0,
         push/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

% api

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec rm_player(integer()) -> {reply, ok, map()}.
rm_player(SessionID) -> 
    gen_server:call(?SERVER, {rm_player, SessionID}).

%Insert new input
-spec push(map(), integer()) -> {reply, ok, map()}.
push(Msg, SessionID) ->
    gen_server:call(?SERVER, {push, Msg, SessionID}).

% The input server should return a single input for every player per invocation
-spec pop() -> list().
pop() ->
   gen_server:call(?SERVER, pop).


% callbacks

init([]) ->
    {ok, 
        #{ } % empty map, where keys are session IDs and values are inputs
    }.

handle_call({rm_player, SessionID}, _From, Players) ->
    {reply, ok, maps:delete(SessionID, Players)};
handle_call(pop, _From, Players) ->
    % Get one input from all players and update the player map with the
    % remaining inputs
    F = 
        fun
            (_ID, [], Acc) ->
                % Nothing to do
                Acc;
            (ID,[Input|Rest],Acc) ->
                % Store the sessionID with the remaining inputs
                Temp = #{ ID => Rest },
                % Build a list of a single input and the session ID
                OldestInput = {ID,Input},
                % Add this to two lists in the accumulator
                #{ player_buf := PlayerBuf, inputs := Inputs } = Acc,
                PlayerBuf1 = maps:merge(PlayerBuf, Temp),
                Inputs1 = [ OldestInput | Inputs ],
                Acc#{ player_buf => PlayerBuf1, inputs => Inputs1 }
        end,
    InputsAndRest = maps:fold(F, #{player_buf => #{}, inputs => []}, Players),
    #{ player_buf := Players1, inputs := Inputs } = InputsAndRest,
    {reply, Inputs, Players1};
handle_call({push, Msg, SessionID}, _From, Players) ->
    NewBuf = 
        case maps:get(SessionID, Players, undefined) of
            undefined ->
                [ Msg ];
            Buffer -> 
                [ Msg | Buffer ]
        end,
    {reply, ok, Players#{ SessionID => NewBuf } }.

handle_cast(_Msg, State) ->
    {noreply, State}.
handle_info(_Info, State) ->
    {noreply, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
