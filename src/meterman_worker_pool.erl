% @author jared kofron <jared.kofron@gmail.com>
-module(meterman_worker_pool).

% api
-export([submit_async/1]).

submit_async(F) ->
    {ok, Pid} = supervisor:start_child(meterman_worker_pool_sup, []),
    gen_server:cast(Pid, {do, F}).
