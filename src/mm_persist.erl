% @author jared kofron <jared.kofron@gmail.com>
% @doc provides functions to interface with persistent storage
% @end
-module(mm_persist).

% try to push to postgres.  on failure, store in ltq.
-export([db_or_ltq/1]).

db_or_ltq(SQL) ->
    try
	{ok, C} = pgsql:connect(localhost, [{database, "nancejk"}]),
	{ok, _N} = pgsql:squery(C, SQL),
	ok = pgsql:close(C)
    catch
	Class:Error ->
	    error_logger:error_msg("Problem while writing: ~p:~p~n",
			       [Class,Error]),
	    meterman_ltq:persist(SQL)
    end.
	
