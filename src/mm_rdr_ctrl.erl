% @author jared kofron <jared.kofron@gmail.com>
% @doc provides control over meterman readers.  keeps track
% of who is reading what so that duplicates are avoided.
% @end
-module(mm_rdr_ctrl).
-behavior(gen_server).
-define(SERVER, ?MODULE).

%% api exports
% generate a new meterman
-export([new/3, who/0]).

%% gen_server exports
-export([start_link/0, init/1, code_change/3, terminate/2]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

%% internal state record
-record(state,
	{ readers = dict:new() }
       ).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-spec new(binary(), integer(), atom()) -> ok | {error, term()}.
new(NewIP, NewPort, NewCard) ->	
    {ok, IOSHostData} = hmhj_node_info:new_with(NewIP, NewPort, NewCard),
    gen_server:cast(?SERVER, {spawn, {host_data, IOSHostData}}),
    ok.

-spec who() -> term().
who() ->
    gen_server:call(?SERVER, {who_has, any}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callback defs %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.

terminate(_Rsn, _StateData) ->
    % should maybe terminate all readers?
    ok.

handle_call({who_has, any}, _From, #state{readers=R}=StateData) ->
    {reply, dict:fetch_keys(R), StateData}.

handle_cast({spawn, {host_data, IOSHostData}}, 
	   #state{readers=D}=StateData) ->
    % create if not already created
    NextState = case dict:is_key(IOSHostData, D) of
		    true ->
			StateData;
		    false ->
			{ok, Pid} = 
			    supervisor:start_child(meterman_reader_sup,
						   [IOSHostData]),
			dict:store(IOSHostData, Pid, D)
		end,
    {noreply, NextState}.

handle_info(_Info, StateData) ->
    {noreply, StateData}.
