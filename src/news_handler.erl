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
	{[{{<<"text">>,<<"plain">>, []}, create_paste}], Req, State}.

content_types_accepted(Req, State) ->
	{[{<<"application/json">>, create_paste}],Req, State}.

delete_resource(Req, State) ->
	case Id = get_id(Req) of
		{fail, Error} ->
			cowboy_req:reply(200, #{}, Error, Req),
			false;
		_ ->
			Result = mnesia_news:delete_news(Id),
			Body = result_to_json(Result),
			cowboy_req:reply(200, #{}, Body, Req),	
			true
	end.

create_paste(Req, State) ->
	
	case cowboy_req:method(Req) of
		<<"GET">> -> 			
			Body = get_from_db(Req);			
		
		<<"POST">> -> 						
			Body = save_to_db(Req);			
		
		<<"PUT">> -> 				
			Body = update_db(Req);
		
		_ ->
			Body = <<"{\"error\": \"Method not supported !\"}">>
	end,
    cowboy_req:reply(200, #{}, Body, Req),
	{ok, Req, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Get news
get_from_db(Req) ->
	Id = get_id(Req),
	case  Id of
		{fail, Error} ->
			 Error;
		_ when Id =< 0 ->
			 Resp = mnesia_news:get_all_news(),				
			 convert_response(Resp);
		_ ->
			 Resp = mnesia_news:get_news(Id),
			 convert_response(Resp)
	end.

convert_response(Resp)->
	case Resp of
		{atomic, Result} ->
			Responce = lists:reverse(convert_news(Result, [])),			
			unescaped_html_chars(jsx:encode(Responce));		
		{aborted, Reason} ->
			jsx:encode([{<<"error">>, Reason}])
	end.

%% Save news
save_to_db(Req) ->
	Content = get_content(Req),
	case validate(Content) of
		{fail, Error} ->
			Error;
		_ ->
			Escaped = escape_html_chars(Content),
			Result = mnesia_news:create_news(Escaped),
			result_to_json(Result)
	end.

%% Update news
update_db(Req) ->
	case Id = get_id(Req) of
		{fail, Error} ->
			Error;
		_ when Id > 0 ->
			Content = get_content(Req),
			case validate(Content) of
				{fail, Error} ->
					Error;
				_ ->
					Escaped = escape_html_chars(Content),
					Result = mnesia_news:update_news(Id, Escaped),
					result_to_json(Result)
			end;
		_ -> 
			<<"{\"error\": \"Empty news_id path variable.\"}">>
	end.

%% Validate html
validate(Content) ->
	case validate_html(Content) of
		false -> 
			{fail, jsx:encode([{<<"error">>, <<"Not valid html.">>}])};
		true -> true
	end.

%% Convert responce to json
result_to_json(Resp) ->
	case Resp of
		{atomic, Result} ->
			jsx:encode([{<<"response">>, Result}]);
		{aborted, Reason} ->
			jsx:encode([{<<"error">>, Reason}])
	end.

%% Get data from rest
get_content(Req) ->
	{ok, ReqBody, _} = cowboy_req:read_body(Req),
	[{<<"content">>, Content}] = jsx:decode(ReqBody),
	Content.

get_id(Req) ->
	Number = cowboy_req:binding(paste_id, Req, <<"0">>),
	try erlang:binary_to_integer(Number) 	
	catch
	   error:badarg -> {fail, <<"{\"error\": \"Wrong news_id path variable.\"}">>}
	end.
	
%% Convert db data for rest representation
convert_news({atomic,[]},[]) -> [];
convert_news([], Resp) -> Resp;
convert_news([Head | Tail], Resp) -> 
	{_, Id, Content} = Head,
	convert_news(Tail, [[{<<"id">>, Id},{<<"content">>, Content}] | Resp]).

%% Escape chars
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