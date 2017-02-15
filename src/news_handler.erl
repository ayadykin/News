%% @author Andrei
%% @doc @todo Add description to news_handler.

-module(news_handler).

-import(mnesia_news, [get_all_news/0, get_news/1, create_news/1, update_news/2, delete_news/1]).
-import(validate_html, [validate_html/1]).

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
	Id = get_id(Req),
	case mnesia_news:delete_news(Id) of
		{atomic, ok} -> 
			Body = jsx:encode([{<<"response">>, ok}]);
		{aborted, Reason} ->
			Body = jsx:encode([{<<"response">>, Reason}])
	end,
	cowboy_req:reply(200, #{}, Body, Req).

create_paste(Req, State) ->
	
	case cowboy_req:method(Req) of
		<<"GET">> -> 
			Id = get_id(Req),
			case Id of
				-1 ->
					News = mnesia_news:get_all_news();				
				_ ->
					News = mnesia_news:get_news(Id)
			end,
			
			Resp = lists:reverse(convert(News, [])),			
			Body = unescaped_html_chars(jsx:encode(Resp));
		
		<<"POST">> -> 			
			Content = get_content(Req),				
			Body = save_to_db(Content);			
		
		<<"PUT">> -> 	
			Id = get_id(Req),
			Content = get_content(Req),	
			Body = update_to_db(Id, Content);
		
		_ ->
			Body = <<"{\"error\": \"Unsupport!\"}">>
	end,
    cowboy_req:reply(200, #{}, Body, Req).

%% ====================================================================
%% Internal functions
%% ====================================================================
	
save_to_db(Content) ->
	case validate_html(Content) of
		true->
			Escaped = escape_html_chars(Content),
			case mnesia_news:create_news(Escaped) of
				{atomic, Result} ->
					jsx:encode([{<<"response">>, Result}]);
				{aborted, Reason} ->
					jsx:encode([{<<"error">>, Reason}])
			end;
		false-> 
			jsx:encode([{<<"error">>, <<"Error validate html.">>}])
	end.

update_to_db(Id, Content) ->
	case validate_html(Content) of
		true->
			Escaped = escape_html_chars(Content),
			case mnesia_news:update_news(Id, Escaped) of
				{atomic, Result} ->
					jsx:encode([{<<"response">>, Result}]);
				{aborted, Reason} ->
					jsx:encode([{<<"error">>, Reason}])
			end;
		false-> 
			jsx:encode([{<<"error">>, <<"Error validate html.">>}])
	end.

get_content(Req) ->
	{ok, ReqBody, _} = cowboy_req:read_body(Req),
	[{<<"content">>, Content}] = jsx:decode(ReqBody),
	Content.

get_id(Req) ->
	Number = cowboy_req:binding(paste_id, Req, -1),
	try erlang:binary_to_integer(Number) 
	catch
	   error:badarg -> -1
	end.
	
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

unescaped_html_chars(Bin) ->
	Bin1 = re:replace(Bin, "&lt;", "<", [global]),
	Bin2 = re:replace(Bin1, "&gt;", ">", [global]),
	re:replace(Bin2, "&amp;", "&", [global]).