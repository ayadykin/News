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
			lager:error("Error delete news with id: ", Id),
			cowboy_req:reply(400, #{}, Error, Req),
			{false, Req, State};
		_ ->			
			lager:info("Delete news with id:", Id),
			Result = mnesia_news:delete_news(Id),
			Body = result_to_json(Result),
			Req2 = cowboy_req:set_resp_body(Body, Req),
			{true, Req2, State}
	end.

create_paste(Req, State) ->
	
	case cowboy_req:method(Req) of
		<<"GET">> -> 			
			lager:info("Get news"),
			case Body = get_from_db(Req) of 			
				{fail, Error} ->
					lager:error("Error get news, reason : ", [Error]),
					Req2 = cowboy_req:reply(400, #{}, Error, Req),
					{true, Req2, State};
				_ ->
					{Body, Req, State}
			end;
		<<"POST">> -> 		
			lager:info("Create news"),
			case Body = save_to_db(Req) of 			
				{fail, Error} ->
					lager:error("Error create news, reason : ", [Error]),
					Req2 = cowboy_req:set_resp_body(Error, Req),
					Req3 = cowboy_req:reply(400, Req2),
					{true, Req3, State};
				_ ->
					Req2 = cowboy_req:set_resp_body(Body, Req),
					{true, Req2, State}
			end;
		
		<<"PUT">> -> 		
			lager:info("Update news"),
			case Body = update_db(Req) of 			
				{fail, Error} ->
					lager:error("Error update news, reason : ", [Error]),
					Req2 = cowboy_req:set_resp_body(Error, Req),
					Req3 = cowboy_req:reply(400, Req2),
					{true, Req3, State};
				_ ->
					Req2 = cowboy_req:set_resp_body(Body, Req),
					{true, Req2, State}
			end;
		
		_ ->
			lager:error("Error method not supported"),
			Body = jsx:encode([{<<"error">>, <<"Method not supported.">>}]),
			cowboy_req:reply(400, #{}, Body, Req, State)
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Get news
get_from_db(Req) ->
	Id = get_id(Req),
	case  Id of
		{fail, Error} ->
			 {fail, Error};
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
			{fail, jsx:encode([{<<"error">>, Reason}])}
	end.

%% Save news
save_to_db(Req) ->
	Content = get_content(Req),
	case validate(Content) of
		{fail, Error} ->
			{fail, Error};
		_ ->
			Escaped = escape_html_chars(Content),
			Result = mnesia_news:create_news(Escaped),
			result_to_json(Result)
	end.

%% Update news
update_db(Req) ->
	case Id = get_id(Req) of
		{fail, Error} ->
			{fail, Error};
		_ when Id > 0 ->
			Content = get_content(Req),
			case validate(Content) of
				{fail, Error} ->
					{fail, Error};
				_ ->
					Escaped = escape_html_chars(Content),
					Result = mnesia_news:update_news(Id, Escaped),
					result_to_json(Result)
			end;
		_ -> 
			{fail, jsx:encode([{<<"error">>, <<"Empty news_id path variable..">>}])}
	end.

%% Validate html
validate(Content) ->
	case validate_html(Content) of
		false -> 
			lager:error("Error validete html"),
			{fail, jsx:encode([{<<"error">>, <<"Not valid html.">>}])};
		true -> true
	end.

%% Convert responce to json
result_to_json(Resp) ->
	case Resp of
		{atomic, Result} ->
			jsx:encode([{<<"response">>, Result}]);
		{aborted, Reason} ->
			{fail, jsx:encode([{<<"error">>, Reason}])}
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
	    error:badarg -> 
			lager:error("Error get news_id path variable"),
			{fail, <<"{\"error\": \"Wrong news_id path variable.\"}">>}
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