-module(ks_npc).

-behaviour(gen_server).

-export([start/1, stop/0]).

% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

% public API

start(Number) ->
    Ships = ["normal", "hauler"],
    Pick = lists:nth(rand:uniform(length(Ships)), Ships),
    gen_server:start(?MODULE, [Number, Pick], []).

stop() ->
    gen_server:stop(?MODULE).

%% callback implementation

init([Number, Type]) ->
    % Create a new Overworld session
    Session = ow_session:new(),
    S1 = ow_session:set_pid(self(), Session),
    Handle = "NPC#" ++ integer_to_list(Number),
    State = #{
        % debug
        counter => 0,
        session => S1,
        game_data => ks_zone2:join(#{handle => Handle, ship => Type}, S1)
    },
    {ok, State}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Who, zone_msg, {zone_transfer, _Msg}}, State) ->
    % Create a link between the NPC and the zone. If the zone dies, so should
    % the NPC. TODO: Ensure that NPC death doesn't kill the zone. Supervisor
    % time?
    logger:notice("NPC linking to ~p", [Who]),
    link(Who),
    logger:notice("Handling a zone transfer"),
    {noreply, State};
handle_info({_Who, zone_msg, {zone_snapshot, _Msg}}, State) ->
    % just a target dummy for now
    {noreply, State};
    %#{counter := C, session := S} = State,
    %Keys = ['ROTATE_RIGHT', 'ROTATE_LEFT', 'IMPULSE_FWD', 'IMPULSE_REV'],
    %Input = lists:nth(rand:uniform(length(Keys)), Keys),
    %case C rem 10 of
    %    0 ->
    %        % Do an area search
    %        %Msg = #{range => 100},
    %        %ks_zone2:area_search(Msg, S);
    %        ks_zone2:input(#{keys => [Input]}, S);
    %    _ ->
    %        ok
    %end,
    %{noreply, State#{counter := C + 1}};
handle_info({_Who, zone_msg, {actor, _Msg}}, State) ->
    %logger:notice("Handling a new actor message"),
    {noreply, State};
handle_info({_Who, zone_msg, {part, _Msg}}, State) ->
    %logger:notice("Handling a part message"),
    {noreply, State};
handle_info({_Who, zone_msg, {area_result, Msg}}, #{session := S} = State) ->
    % Filter out myself
    ID = ow_session:get_id(S),
    _Results = [ow_ecs:get(handle, Components) || {_ID, Components} <- lists:keydelete(ID, 1, Msg)],
    {noreply, State};
handle_info({_Who, zone_msg, {admin_reconfig, _Msg}}, State) ->
    % Nothing to do for now
    {noreply, State};
handle_info({_Who, OWType, {Unhandled, _Msg}}, State) ->
    logger:notice("Unhandled message type! ~p:~p", [OWType, Unhandled]),
    {noreply, State}.

terminate(_Reason, #{session := S} = _State) ->
    ks_zone2:part(S),
    ok.

code_change(_OldVsn, _NewVsn, State) ->
    {ok, State}.
