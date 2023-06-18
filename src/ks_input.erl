-module(ks_input).
-behaviour(gen_server).

-define(SERVER(SessionID),
    {via, gproc, {n, l, {?MODULE, SessionID}}}
).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([
		start/3,
		start_link/3,
        stop/1,
        pop/1,
		push/2, 
        apply/4,
        slide/2 %window
		]).
%-export([start_link/1, apply/4, push/3, proc_reset/1, proc_debug/2]).

-record(state, {
			session_id :: ow_session:session_id(),
			buffer = [] :: list(),
            frame_window :: queue:queue(),
            seen_frames = [] % TODO
	}).

%% API 

start(SyncFrame, WindowSize, SessionID) ->
    Args = [SyncFrame, WindowSize, SessionID],
    gen_server:start(?SERVER(SessionID), ?MODULE, Args, []).
start_link(SyncFrame, WindowSize, SessionID) ->
    Args = [SyncFrame, WindowSize, SessionID],
    gen_server:start_link(?SERVER(SessionID), ?MODULE, Args, []).

stop(SessionID) ->
    gen_server:stop(?SERVER(SessionID)).

% Pop the oldest input 
pop(SessionID) ->
    gen_server:call(?SERVER(SessionID), pop).

% Insert new input
push(Msg, SessionID) ->
	gen_server:call(?SERVER(SessionID), {push, Msg}).

% Recurse over all of the inputs buffered this tick.
-spec apply(list(), map(), ow_ecs2:world(), integer()) -> ok.
apply(Actions, ZoneData, World, SessionID) ->
	gen_server:call(?SERVER(SessionID), {apply, Actions, ZoneData, World}).

slide(WindowSize, SessionID) ->
    gen_server:call(?SERVER(SessionID), {slide, WindowSize}).

% Callbacks
init([SyncFrame, WindowSize, SessionID]) ->
    Queue = initialize_queue(WindowSize, SyncFrame),
	InitialState = #state{
						session_id=SessionID,
						buffer=ordsets:new(),
                        frame_window=Queue
					},
    {ok, InitialState}.

handle_call(pop, _From, #state{ buffer = Buffer0 } = State) ->
    [ Input | Rest ] = Buffer0,
    {reply, Input, State#state{ buffer = Rest }};
handle_call({push, Msg}, _From, #state{ buffer = Buffer0, frame_window = FrameWindow } = State) ->
	% If we're gonna crash from bad input, now's the time to crash with minimal
	% collateral damage!
	% Store the input into the buffer, keying off of the frame
    Frame = maps:get(frame, Msg),
    Input = {Frame, maps:with([keys, cursor], Msg)},
	% Check if the input is in the valid window.  This is not foolproof. The
	% window slides forward each frame, allowing up to N frames of duplication
	% where N is the size of the window. While enet should ensure that frames
	% arrive reliably and in order, we need to also track "seen_frames" and
	% ensure no client is sending duplicate frames intentionally.
    case queue:member(Frame, FrameWindow) of
        true -> 
            Buffer1 = ordsets:add_element(Input, Buffer0),
            {reply, ok, State#state{buffer=Buffer1}};
        false ->
            Max = queue:peek_r(FrameWindow),
            Min = queue:peek(FrameWindow),
            logger:notice("Input out of range: ~p [~p-~p]", [Frame, Min, Max]),
            {reply, invalid_frame, State}
    end;
handle_call({apply, Actions, ZoneData, World}, _From, State) ->
    SessionID = State#state.session_id,
    % Get any valid inputs living in the buffer
    Buffer = State#state.buffer,
    % Pop the oldest input off the buffer and apply it
    logger:notice("popping one input off of Buffer: ~p", [Buffer]),
    Buffer1 = pop_buffer(Buffer, Actions, ZoneData, World, SessionID),
    % Report remaining items and update the buffer
	{reply, {ok, length(Buffer)}, State#state{buffer=Buffer1}};
handle_call({slide, Window}, _From, #state{ frame_window = Queue } = State) ->
    %TODO: I might become a bottleneck! Maybe change to cast later.
    Queue1 = slide_window(Queue, Window),
    {reply, ok, State#state{ frame_window = Queue1 }};
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

%handle_cast({slide, Window}, #state{ frame_window = Queue } = State) ->
%    Queue1 = slide_window(Queue, Window),
%    logger:notice("new Queue: ~p", [Queue1]),
%    {noreply, State#state{ frame_window = Queue1 }};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% internal functions
%process_buffer([], _Actions, _ZoneData, _World, _SessionID) ->
%    ok;
%process_buffer([{_Frame, InputMap} |Rest], Actions, ZoneData, World, SessionID) ->
%    #{ cursor := Cursor, keys := Keys } = InputMap,
%    % Make sure that no keys got doubled up input during this frame
%    UniqKeys = lists:uniq(Keys),
%    % Execute the given function in the Action key table for this caller
%    Apply =
%        fun(KeyPress) ->
%            case lists:keyfind(KeyPress, 1, Actions) of
%                {KeyPress, Fun} ->
%                    Fun(SessionID, Cursor, ZoneData, World);
%                false ->
%                    ok
%            end
%        end,
%    lists:foreach(Apply, UniqKeys),
%    process_buffer(Rest, Actions, ZoneData, World, SessionID).

pop_buffer([], _Actions, _ZoneData, _World, _SessionID) ->
    [];
pop_buffer([{_Frame, InputMap} |Rest], Actions, ZoneData, World, SessionID) ->
    #{ cursor := Cursor, keys := Keys } = InputMap,
    % Make sure that no keys got doubled up input during this frame
    UniqKeys = lists:uniq(Keys),
    % Execute the given function in the Action key table for this caller
    Apply =
        fun(KeyPress) ->
            case lists:keyfind(KeyPress, 1, Actions) of
                {KeyPress, Fun} ->
                    Fun(SessionID, Cursor, ZoneData, World);
                false ->
                    ok
            end
        end,
    lists:foreach(Apply, UniqKeys),
    Rest.

initialize_queue(Window, SyncFrame) ->
    EmptyQueue = queue:new(),
    InitialRange = lists:seq(SyncFrame-Window,SyncFrame+Window),
    F = fun(Elem, Q) ->
            queue:in(Elem, Q)
        end,
    lists:foldl(F, EmptyQueue, InitialRange).

slide_window(Queue, Max) ->
    % Slide the window forward by 1 frame
    {value, Frame} = queue:peek_r(Queue),
    case queue:len(Queue) =< Max*2 of
        % Add this frame to the queue
        true ->
            queue:in(Frame+1, Queue);
        false ->
            Q2 = queue:drop(Queue),
            queue:in(Frame+1, Q2)
    end.



%% Recurse over all of the inputs buffered this tick.
%-spec apply(list(), integer(), list(), ow_ecs2:world()) -> ok.
%apply([], _ID, _KeyList, _World) ->
%    ok;
%apply([Input | Rest] = All, ID, Actions, World) ->
%    %logger:notice("Input this frame: ~p", [Input]),
%    Cursor = maps:get(cursor, Input, undefined),
%    KeyList = maps:get(keys, Input, undefined),
%    % Delete duplicate keys, preseve the order.
%    KeyList1 = lists:uniq(KeyList),
%    Apply =
%        fun(KeyPress) ->
%            case lists:keyfind(KeyPress, 1, Actions) of
%                {KeyPress, Fun} ->
%                    Fun(ID, Cursor, World);
%                false ->
%                    ok
%            end
%        end,
%    lists:foreach(Apply, KeyList1),
%    % Mode 1: Push excess inputs off to the next frame.
%    % We've applied all we can this frame. Push the rest off to the next frame.
%    %case Rest of
%    %    [] -> ok;
%    %    _ -> 
%    %        logger:notice("Extra input this frame: ~p", [Rest])
%    %end,
%    %ow_ecs2:add_component(input, Rest, ID, World).
%    % Mode 2: Process all inputs this frame and let the client catch up
%    apply(Rest, ID, Actions, World).
%
%log_input(Input, ID, World) ->
%    Map =
%        case ow_ecs2:try_component(log_input, ID, World) of
%            false -> #{};
%            Components -> ow_ecs2:get(log_input, Components)
%        end,
%    % Classify the input
%    % Input can be a list of keys
%    #{keys := Keys} = Input,
%    % Fold over the list of keys this frame
%    F = fun(KeyType, MapIn) ->
%        Previous = maps:get(KeyType, MapIn, 0),
%        MapIn#{KeyType => Previous + 1}
%    end,
%    Map1 = lists:foldl(F, Map, Keys),
%    ow_ecs2:add_component(log_input, Map1, ID, World).
%
%proc_reset(World) ->
%    % Get all entities with input
%    E = ow_ecs2:match_component(input, World),
%    [ow_ecs2:del_component(input, ID, World) || {ID, _} <- E].
%
%proc_debug(#{frame := Frame}, World) ->
%    if
%        Frame rem 100 == 0 ->
%            E = ow_ecs2:match_component(log_input, World),
%            F =
%                fun({_ID, Components}) ->
%                    Log = ow_ecs2:get(log_input, Components),
%                    logger:notice("Input stats: ~p~n", [Log])
%                end,
%            [F(Match) || Match <- E];
%        true ->
%            ok
%    end.

