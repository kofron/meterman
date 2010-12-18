-module(meterman_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    SupStrat = {one_for_one, 5, 10},
    MMReaderSup = {meterman_reader_sup,
		   {meterman_reader_sup, start_link, []},
		   permanent,
		   1000,
		   supervisor,
		   [meterman_reader_sup]},
    WPSup = {meterman_worker_pool_sup,
	     {meterman_worker_pool_sup, start_link, []},
	     permanent,
	     1000,
	     supervisor,
	     [meterman_worker_pool_sup]},
    SVCSup = {meterman_svc_sup,
	      {meterman_svc_sup, start_link, []},
	      permanent,
	      10000,
	      supervisor,
	      [meterman_svc_sup]},
    Children = [WPSup, SVCSup, MMReaderSup],
    {ok, {SupStrat, Children}}.

