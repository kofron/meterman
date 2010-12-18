% @author jared kofron <jared.kofron@gmail.com>
-module(meterman_worker).
-behavior(gen_server).

% state record
-record(state, {}).

% gen_server exports
-export([start_link/0, init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_cast/2, handle_call/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server exports %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

terminate(_Rsn, _StateData) ->
    ok.

code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.

handle_call({do, Fcn}, _From, StateData) ->
    {reply, Fcn(), StateData, 0}.

handle_cast({do, Fcn}, StateData) ->
    Fcn(),
    {noreply, StateData, 0}.

handle_info(timeout, StateData) ->
    {stop, normal, StateData}.
