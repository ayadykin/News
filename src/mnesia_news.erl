%% @author Andrei
%% @doc @todo Add description to news_handler.

-module(mnesia_news).

-include("news.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([get_all_news/0, get_news/1, create_news/1, update_news/2, delete_news/1]).


get_all_news() ->
	do(qlc:q([X || X <- mnesia:table(news)])).

get_news(Id) ->
	{atomic, News} = mnesia:transaction(fun() -> mnesia:read(news, Id) end),
	News.

create_news(Content) ->
	Id = mnesia:dirty_update_counter(unique_ids, news_id, 1),
	News = #news{id = Id, content = Content},
	mnesia:transaction(fun() -> mnesia:write(News) end).

update_news(Id, Content) ->
	News = #news{id = Id, content = Content},
	mnesia:transaction(fun() -> mnesia:write(News) end).

delete_news(Id) ->
	mnesia:transaction(fun() -> mnesia:delete({news, Id}) end).

%% ====================================================================
%% Internal functions
%% ====================================================================

do(Q) ->
    F = fun() -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.