%%%-------------------------------------------------------------------
%% @doc news_webserver top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(news_webserver_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	 UsrChild = {news_server,{news_server, start_link, []},
              permanent, 2000, worker, [news_server]},
    {ok,{{one_for_all,1,1}, [UsrChild]}}.

%%====================================================================
%% Internal functions
%%====================================================================
