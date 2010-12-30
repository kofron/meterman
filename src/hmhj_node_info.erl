% @author jared kofron <jared.kofron@gmail.com>
% @doc Data structure module for hmhj_node_info.
% provides an interface to data about where a
% data server lives, including the port that the readers
% should connect to to collect data.
% @end
-module(hmhj_node_info).

% node info record
-record(nodeinfo,
	{ip = <<0,0,0,0>> :: binary(),
	 port = 8000 :: integer(),
	 card = <<"cardA">> :: binary(),
	 host = <<"ioserver">> :: binary()
	}).

% api
-export([new/0, new_with/3, get_ip/1, get_port/1, get_card/1, to_uri/1]).
-export([get_host/1]).
-export([is_node_info/1]).
new() ->
    #nodeinfo{}.

is_node_info(Blob) when is_record(Blob, nodeinfo) ->
    true;
is_node_info(_) ->
    false.

new_with(IPAddr, Port, Card) ->
    {ok, #nodeinfo{ip=IPAddr, port=Port, card=Card}}.

get_ip(Blob) ->
    Blob#nodeinfo.ip.

get_port(Blob) ->
    Blob#nodeinfo.port.

get_card(Blob) ->
    Blob#nodeinfo.card.

get_host(Blob) ->
    Blob#nodeinfo.host.

to_uri(Blob) ->
    {ok, Host} = binary_to_url(get_ip(Blob)),
    PortStr = integer_to_list(get_port(Blob)),
    Card = atom_to_list(get_card(Blob)),
    URI = ["http://",Host, ":",PortStr,"/data/",Card],
    lists:flatten(URI).

binary_to_url(<<A,B,C,D>>) ->
    {ok, [integer_to_list(A),
	  ".",
	  integer_to_list(B),
	  ".",
	  integer_to_list(C),
	  ".",
	  integer_to_list(D)]
    }.
