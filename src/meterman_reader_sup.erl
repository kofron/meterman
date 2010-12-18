% @author jared kofron <jared.kofron@gmail.com>
% @copyright 2010 jared kofron
% @doc Supervises the readers which are spawned
% to handle individual I/O server connections.
% @end						
-module(meterman_reader_sup).
-behavior(supervisor).

-define(SUPERVISOR, ?MODULE).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

init(_Args) ->
    SupStrat = {simple_one_for_one, 5, 10},
    CSpec = {meterman_reader,
	     {meterman_reader, start_link, []},
	     transient,
	     brutal_kill,
	     worker,
	     [meterman_reader]},
    {ok, {SupStrat, [CSpec]}}.
    
