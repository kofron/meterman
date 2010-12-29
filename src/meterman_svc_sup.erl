% @author jared kofron <jared.kofron@gmail.com>
-module(meterman_svc_sup).
-behavior(supervisor).

% supervisor name
-define(SUPERVISOR, ?MODULE).

% supervisor exports
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

init([]) ->
    SuperStrat = {one_for_one, 5, 10},
    LTQSpec = {meterman_ltq, 
	       {meterman_ltq, start_link, []},
	       permanent,
	       5000,
	       worker,
	       [meterman_ltq]},
    CTRLSpec = {mm_rdr_ctrl,
		{mm_rdr_ctrl, start_link, []},
		permanent,
		5000,
		worker,
		[mm_rdr_ctrl]},
    Children = [LTQSpec, CTRLSpec],
    {ok, {SuperStrat, Children}}.
