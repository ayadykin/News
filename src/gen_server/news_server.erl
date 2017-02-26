%% @author Andrei
%% @doc @todo Add description to news_server.

-module(news_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_all_news/0]).
-export([start_link/0]).

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% News Services API
get_all_news() ->
	lager:info("get_news()"),
	gen_server:call(?MODULE, get_all_news).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([]) ->
    {ok, #state{}}.

%% Callback Functions
handle_call(get_all_news, _From, LoopData) ->
  Reply = mnesia_news:get_all_news(),
  lager:info("handle_call -> get_news : ", Reply),
  {reply, Reply, LoopData};

handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
	lager:error("handle_info : ", Info),
    {noreply, State}.


terminate(Reason, State) ->
    ok.

code_change(OldVsn, State, Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================


