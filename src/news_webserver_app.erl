%%%-------------------------------------------------------------------
%% @doc news_webserver public API
%% @end
%%%-------------------------------------------------------------------

-module(news_webserver_app).

-behaviour(application).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/static", cowboy_static, {file, "priv/index.html"}},
			{"/news/[:paste_id]", news_handler, []},	
			{"/news", news_handler, []}			
		]}
	]),
	 {ok, _} = cowboy:start_clear(http, 100, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
    news_webserver_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
