% @author jared kofron <jared.kofron@gmail.com>
% @doc provides parse transform functions to get json data
% into more useful internal formats 
% @end
-module(mm_pt).

% convert from json to mm_data
-export([j2d/1]).

% convert from mm_data to list of sql statements
-export([d2s/1]).

j2d(JSON) ->
    try 
	DJ = mochijson2:decode(JSON),
	{data, dj2d(DJ)}
    catch
	C:E ->
	    io:format("~p: ~p~n",[C,E])
    end.

dj2d(DJ) ->
    dj2d(DJ, mm_data:new()).
dj2d({struct, PL}, DBlob) ->
    dj2d(PL, DBlob);
dj2d([{<<"card",_SlotLetter:1/binary>>=Card, {struct, PL}}], D) ->
    {ok, ND} = mm_data:set_card(D,Card),
    dj2d(PL,ND);
dj2d([{struct, PL}|_R], DBlob) ->
    dj2d(PL, DBlob);
dj2d([{<<"timestamp">>,V}|R],DBlob) ->
    {ok, NDBlob} = mm_data:set_ts(DBlob, V),
    dj2d(R,NDBlob);
dj2d([{<<"channel",N:1/binary>>,
       {struct,[{<<"voltage">>,V}]}} | R], D) ->
    ChNum = list_to_integer(binary_to_list(N)),
    {ok, ND} = mm_data:append_data(D,{ChNum,V}),
    dj2d(R,ND);
dj2d([{<<"channel",N1:1/binary,N2:1/binary>>,
       {struct,[{<<"voltage">>,V}]}} | R], D) ->
    ChNum = 10*list_to_integer(binary_to_list(N1)) + 
	list_to_integer(binary_to_list(N2)),
    {ok, ND} = mm_data:append_data(D,{ChNum,V}),
    dj2d(R,ND);
dj2d([], DBlob) ->
    DBlob.

d2s(MMData) ->
    H = hmhj_node_info:get_host(mm_data:get_source(MMData)),
    T = integer_to_list(mm_data:get_ts(MMData)),
    D = mm_data:get_chdat(MMData),
    C = mm_data:get_card(MMData),
    d2s(H,C,T,D,[]).
d2s(_HostName, _CardSlot, _Timestamp, [], Acc) ->
    Acc;
d2s(HostName, CardSlot, Timestamp, [{Ch,V}|R], Acc) ->
    Val = case V of
	      N when is_integer(N) ->
		  integer_to_list(N);
	      M when is_float(M) ->
		  float_to_list(M)
	  end,
    Str = [<<"insert into master">>,
	   <<" ">>,
	   <<"(hostname, cardslot, timestamp, channel, value)">>,
	   <<" ">>,
	   <<"values">>,
	   <<" ">>,
	   <<"(">>,
	   escape(HostName),
	   <<",">>,
	   escape(CardSlot),
	   <<",">>,
	   Timestamp,
	   <<",">>,
	   integer_to_list(Ch),
	   <<",">>,
	   Val,
	   <<")">>],
    d2s(HostName,CardSlot,Timestamp,R,[Str|Acc]).

escape(Bin) ->
    lists:foldr(fun(A,B) -> 
			<<A/binary,B/binary>> 
		end, 
		<<>>,
		[<<$'>>,Bin,<<$'>>]).
			
