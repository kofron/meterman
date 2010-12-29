% @author jared kofron <jared.kofron@gmail.com>
-module(meterman_reader).
-behavior(gen_server).

% Timeout interval
-define(TMOUT, 1000).
-define(TMOUTUS, 1000*?TMOUT).

% internal state record
-record(state, {
	  tgt = nobody
	 }).

% gen_server exports
-export([start_link/0, start_link/1, start_link/3, init/1]).
-export([terminate/2, code_change/3]).
-export([handle_call/3, handle_info/2, handle_cast/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gen_server Callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link(?MODULE, [], []).

start_link(IOSHostData) ->
    gen_server:start_link(?MODULE, [IOSHostData], []).

start_link(TgtIp, TgtPort, TgtCard) ->
    {ok, IOSHostData} = hmhj_node_info:new_with(TgtIp, TgtPort, TgtCard),
    gen_server:start_link(?MODULE, [IOSHostData], []).

init([]) ->
    TgtData = hmhj_node_info:new(),
    {ok, #state{tgt=TgtData}, ?TMOUT};
init([TgtData]) ->
    {ok, #state{tgt=TgtData}, ?TMOUT}.

terminate(_Rsn, _StateData) ->
    ok.

code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.

handle_call(Call, _From, StateData) ->
    {stop, {unexpected_call, Call}, StateData}.

handle_cast(Cast, StateData) ->
    {stop, {unexpected_cast, Cast}, StateData}.

handle_info(timeout, #state{tgt=Tgt}=StateData) ->
    StartTS = erlang:now(),
    URI = hmhj_node_info:to_uri(Tgt),
    Data = get_hmhj_data(URI),
    case mm_pt:j2d(Data) of
	{data, DBlob} ->
	    {ok, NDBlob} = mm_data:set_host(DBlob, Tgt),
	    F = fun() -> io:format("sql statement:~p~n",
				   [lists:flatten(mm_pt:d2s(NDBlob))])
		end,
	    meterman_worker_pool:submit_async(F);
	{error, Err} ->
	    io:format("decoding error: ~p~n",[Err])
    end,
    SleepTime = case timer:now_diff(erlang:now(), StartTS) of
		    T when T > ?TMOUTUS ->
			0;
		    T when T < ?TMOUTUS ->
			trunc(?TMOUT - T/1000)
		end,
    {noreply, StateData, SleepTime}.

%%%%%%%%%%%%%%%%
%%% Internal %%%
%%%%%%%%%%%%%%%%
get_hmhj_data(URI) ->
    case httpc:request(URI) of
	{ok, {_StatCode, _Body}} ->
	    retry;
	{ok, {{_HTTPVer, 200, "OK"}, _Hdr, Body}} ->
	    Body;
	{error, _Err} ->
	    noretry
    end.
