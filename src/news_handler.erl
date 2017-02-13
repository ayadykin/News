%% @author Andrei
%% @doc @todo Add description to news_handler.

-module(news_handler).

-import(mnesia_news, [get_all_news/0, get_news/1, create_news/1, update_news/2, delete_news/1]).

%% ====================================================================
%% API functions
%% ====================================================================

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([create_paste/2]).
-export([delete_resource/2]).

init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
	{[		
		{{<<"text">>, <<"plain">>, []}, create_paste}
	], Req, State}.

content_types_accepted(Req, State) ->
	{[
	  {<<"application/json">>, create_paste}
	 ],Req, State}.

delete_resource(Req, State) ->
	N = cowboy_req:binding(paste_id, Req, -1),
	case mnesia_news:delete_news(N) of
		{atomic, ok} -> 
			Body = jsx:encode([{<<"response">>, ok}]);
		{aborted, Reason} ->
			Body = jsx:encode([{<<"response">>, Reason}])
	end,
	{ok, Req1} = cowboy_req:reply(200, #{}, Body, Req),
    {ok, Req1, State}.

create_paste(Req, State) ->
	N = cowboy_req:binding(paste_id, Req, -1),
	
	case cowboy_req:method(Req) of
		<<"GET">> -> 
			case N of
				-1 ->
					News = mnesia_news:get_all_news();				
				_ ->
					News = mnesia_news:get_news(N)
			end,
			
			Resp = lists:reverse(convert(News,[])),			
			Body = jsx:encode(Resp);
		
		<<"POST">> -> 
			{ok, ReqBody, _} = cowboy_req:read_body(Req),
        	Req_Body_decoded = jsx:decode(ReqBody),
        	[{<<"content">>, Content}] = Req_Body_decoded,
			Escaped = escape_html_chars(Content),
			case mnesia_news:create_news(Escaped) of
				{atomic, Result} ->
					Body = jsx:encode([{<<"response">>, Result}]);
				{aborted, Reason} ->
					Body = jsx:encode([{<<"error">>, Reason}])
			end;
		
		<<"PUT">> -> 	
			{ok, ReqBody, _} = cowboy_req:read_body(Req),
        	Req_Body_decoded = jsx:decode(ReqBody),
        	[{<<"id">>, Id}, {<<"content">>, Content}] = Req_Body_decoded,
			

			case mnesia_news:update_news(Id, Content) of
				{atomic, Result} ->
					Body = jsx:encode([{<<"response">>, Result}]);
				{aborted, Reason} ->
					Body = jsx:encode([{<<"error">>, Reason}])
			end;
		
		_ ->
			Body = <<"{\"rest\": \"Unsupport!\"}">>
	end,
    {ok, Req1} = cowboy_req:reply(200, #{}, Body, Req),
    {ok, Req1, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

convert({atomic,[]},[]) -> [];
convert([], Res) -> Res;
convert([Head | Tail], Res) -> 
	{_, Id, Content} = Head,
	convert(Tail, [[{<<"id">>, Id},{<<"content">>, Content}] | Res]).

escape_html_chars(Bin) ->
	<< <<(escape_html_char(B))/binary>> || <<B>> <= Bin >>.

escape_html_char($<) -> <<"&lt;">>;
escape_html_char($>) -> <<"&gt;">>;
escape_html_char($&) -> <<"&amp;">>;
escape_html_char(C) -> <<C>>.