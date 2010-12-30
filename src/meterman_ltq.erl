% @author jared kofron <jared.kofron@gmail.com>
% @doc Provides an interface to a 'long term queue'.
% This is a place to stash data that can't be written
% to the database right away.  If the queue grows
% beyond a certain length, it will write itself to disk.
% On startup, it will try to recover from disk.
% @end
-module(meterman_ltq).
-behavior(gen_server).

% name of the server
-define(SERVER, ?MODULE).

% retry timeouts
-define(TMOUT_WT, 5000).

% internal state record
-record(state,
	{
	  q = queue:new()
	}).

% api
-export([persist/1, persist_async/1, next_to_write/0, length/0]).

% gen_server exports
-export([start_link/0, init/1, terminate/2, code_change/3]).
-export([handle_info/2, handle_call/3, handle_cast/2]).

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
persist(Item) ->
    gen_server:call(?SERVER, {store, Item}).

persist_async(Item) ->
    gen_server:cast(?SERVER, {store, Item}).

next_to_write() ->
    gen_server:call(?SERVER, next).

length() ->
    gen_server:call(?SERVER, length).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gen_server callbacks %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    NewQ = maybe_recover(),
    {ok, #state{q=NewQ}}.

terminate(_Reason, #state{q=Q}=_State) ->			
    maybe_write_q(Q),
    ok.

code_change(_OldVsn, StateData, _Extra) ->	    
    {ok, StateData}.

handle_call({store, Item}, _From, #state{q=Q}=StateData) ->
    {ok, NewQ} = write_to_queue(Q, Item),
    {reply, ok, StateData#state{q=NewQ}, ?TMOUT_WT};
handle_call(next, _From, #state{q=Q}=StateData) ->
    Next = case queue:peek(Q) of
	       {value, Item} ->
		   Item;
	       empty ->
		   []
	   end,
    {reply, Next, StateData, ?TMOUT_WT};    
handle_call(length, _From, #state{q=Q}=StateData) ->
    {reply, queue:len(Q), StateData}.

handle_cast({store, Item}, #state{q=Q}=StateData) ->
    {ok, NewQ} = write_to_queue(Q, Item),
    {noreply, StateData#state{q=NewQ}, ?TMOUT_WT}.

handle_info(timeout, #state{q=Q}=StateData) ->
    maybe_write_q(Q),
    {noreply, StateData, ?TMOUT_WT}.

%%%%%%%%%%%%%%%%
%%% Internal %%%
%%%%%%%%%%%%%%%%
write_to_queue(Q, Item) ->
    {ok, queue:in(Item, Q)}.

maybe_recover() ->
    case file:read_file(filename()) of
	{ok, <<>>} ->
	    queue:new();
	{ok, Data} ->
	    valid_or_new_queue(Data);
	{error, enoent} ->
	    queue:new()
    end.

valid_or_new_queue(Data) ->
    MaybeQ = binary_to_term(Data),
    Return = case queue:is_queue(MaybeQ) of
		 true ->
		     MaybeQ;
		 false ->
		     queue:new()
	     end,
    Return.

filename() ->
    "./priv/queued.raw".

maybe_write_q(Q) ->
    case queue:is_empty(Q) of
	true ->
	    ok;
	false ->
	    write_q(Q)
    end.

write_q(Q) ->
    BData = term_to_binary(Q),
    ok = file:write_file(filename(),BData).
    
	    
