-module(news_webserver).

-include("news.hrl").
-behaviour(application).

-define(APPS, [crypto, asn1, public_key, ssl,  ranch, cowlib, cowboy, mnesia, jsx]).

%% ===================================================================
%% API functions
%% ===================================================================

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	ensure_started(?APPS),
	start_cowboy(),
	create_mnesia_tables(),
	sync:go(),
	
    news_webserver_sup:start_link().

stop(_State) ->
    sync:stop(),
	stop_apps(lists:reverse(?APPS)).

%% ===================================================================
%% Internal functions
%% ===================================================================

ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
	case application:start(App) of
		ok -> 
			ensure_started(Apps);
		{error, {already_started, App}} -> 
			ensure_started(Apps)
	end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
	application:stop(App),
	stop_apps(Apps).

start_cowboy()->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/static", cowboy_static, {file, "priv/index.html"}},
			{"/news/[:paste_id]", news_handler, []},	
			{"/news", news_handler, []}			
		]}
	]),
	 {ok, _} = cowboy:start_clear(http, 100, [{port, 8080}], #{env => #{dispatch => Dispatch}}).
	
create_mnesia_tables() ->
	lager:start(),
	 mnesia:create_table(unique_ids, [{attributes, record_info(fields, unique_ids)}] ),
	 mnesia:create_table(news, [{attributes, record_info(fields, news)}]).

