% @author jared kofron <jared.kofron@gmail.com>
% @doc abstract data module which provides and modifies mm_data 
% type.
% @end
-module(mm_data).

% for now, mm_data is a record
-record(data, {
	  card = none :: atom(),
	  source = none :: atom(),
	  ts = 0 :: integer(),
	  nch  = 0 :: integer(),
	  chdat = [] :: [float() | integer()]
			}).

% valid card atoms
-type validcard() :: cardA | cardB | cardC | cardD.

% constructors
-export([new/0]).

% getters/setters
-export([get_card/1, get_source/1, get_nch/1, get_chdat/1, get_ts/1]).
-export([set_card/2, set_source/2, set_nch/2, set_chdat/2, set_ts/2]).

% convenience methods
-export([append_data/2]). % adds a channel and increments channel count

%%%%%%%%%%%
%%% API %%%
%%%%%%%%%%%
-spec new() -> #data{}.
new() ->
    #data{}.

-spec get_card(#data{}) -> validcard().
get_card(#data{card=C}) ->
    C.

-spec set_card(#data{}, validcard()) -> {ok, #data{}}.
set_card(D, C) when is_record(D, data) ->
    {ok, D#data{card = C}}.

-spec get_source(#data{}) -> atom().
get_source(#data{source=H}) ->
    H.

-spec set_source(#data{}, atom()) -> {ok, #data{}}.
set_source(D, H) when is_record(D, data) ->
    {ok, D#data{source=H}}.

-spec get_nch(#data{}) -> integer().
get_nch(#data{nch=N}) ->
    N.

-spec set_nch(#data{}, integer()) -> {ok, #data{}}.
set_nch(D, N) when is_record(D, data) ->
    {ok, D#data{nch=N}}.

-spec get_chdat(#data{}) -> [float() | integer()].
get_chdat(#data{chdat=C}) ->
    C.

-spec set_chdat(#data{}, [float() | integer()]) -> {ok, #data{}}.
set_chdat(D, C) when is_record(D, data) ->
    {ok, D#data{chdat = C}}.

-spec get_ts(#data{}) -> integer().
get_ts(#data{ts=T}) ->
    T.

-spec set_ts(#data{}, integer()) -> {ok, #data{}}.
set_ts(D, T) when is_record(D, data) ->
    {ok, D#data{ts=T}}.

-spec append_data(#data{}, integer() | float()) -> {ok, #data{}}.
append_data(D, P) when is_record(D, data) ->
    ChCount = mm_data:get_nch(D),
    CData = mm_data:get_chdat(D),
    {ok, D#data{nch = ChCount + 1, 
	   chdat = CData ++ [P]}}.
