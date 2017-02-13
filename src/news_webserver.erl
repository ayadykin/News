-module(news_webserver).

-record( unique_ids, {type, id} ).

-include("news.hrl").

-define(APPS, [crypto, asn1, public_key, ssl,  ranch, cowlib, cowboy, news_webserver, mnesia, jsx]).

%% ===================================================================
%% API functions
%% ===================================================================

-export([start/0,stop/0]).

start() ->
	ok = ensure_started(?APPS),
	init(),
	ok = sync:go().

stop() ->
	sync:stop(),
	ok = stop_apps(lists:reverse(?APPS)).

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
	case application:start(App) of
		ok -> ensure_started(Apps);
		{error, {already_started, App}} -> ensure_started(Apps)
	end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
	application:stop(App),
	stop_apps(Apps).

init() ->
	mnesia:create_table( unique_ids, [{attributes, record_info(fields, unique_ids)}] ),
	 mnesia:create_table(news, [{attributes, record_info(fields, news)}]).

