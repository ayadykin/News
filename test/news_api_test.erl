%% @author Andrei
%% @doc @todo Add description to news_api_test.

-module(news_api_test).
-include_lib("eunit/include/eunit.hrl").

efrisby_test_() ->
    {setup,
        fun setup/0,
        fun teardown/1,
        [
            fun get_request/0
        ]
    }.

setup() ->
	application:start(news_webserver),
    hackney:start().

teardown(_) ->
	application:stop(news_webserver),
	hackney:stop().

get_request() ->
	efrisby:get("http://localhost:8080/news", [
    	{status, 200},
		{content_type, "application/json; charset=utf-8"},
	    {json_types, [
	        {<<"content">>, list}
	    ]},
	    {json, [
	        {<<"content">>, []}
	    ]}
	]).
