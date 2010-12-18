% @author jared kofron <jared.kofron@gmail.com>
-module(meterman_worker_pool_sup).
-behavior(supervisor).

% supervisor name
-define(SUPERVISOR, ?MODULE).

% supervisor exports
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

init([]) ->
    SuperStrat = {simple_one_for_one, 5, 10},
    ChildSpec  = {meterman_worker,
		  {meterman_worker, start_link, []},
		  temporary,
		  brutal_kill,
		  worker,
		  [meterman_worker]},
    {ok, {SuperStrat, [ChildSpec]}}.
