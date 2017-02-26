%% @author Andrei
%% @doc @todo Add description to news_handler.

-module(news_handler).

%% ====================================================================
%% API functions
%% ====================================================================

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([create_paste/2]).
-export([delete_resource/2]).
-export([resource_exists/2]).

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
		_ when Id > 0 ->			
			lager:info("Delete news with id:", Id),
			Result = mnesia_news:delete_news(Id),
			success_response(Result, Req, State); 
		_ ->			
			lager:error("Error delete news with id: ", Id),
			error_response(<<"Empty news_id path variable.">>, Req, State)
	end.

create_paste(Req, State) ->
	
	case cowboy_req:method(Req) of
		<<"GET">> -> 			
			lager:info("Get news"),
			case Body = get_from_db(Req) of 			
				{fail, Error} ->
					lager:error("Error get news, reason : ", [Error]),
					error_response(Error, Req, State);
				_ ->
					{Body, Req, State}
			end;
		<<"POST">> -> 		
			lager:info("Create news"),
			Body = save_to_db(Req),
			erlang: display (Body),
			case Body of 			
				{fail, Error} ->
					lager:error("Error create news, reason : ", [Error]),
					error_response(Error, Req, State);
				_ ->
					success_response(Body, Req, State)
			end;
		
		<<"PUT">> -> 		
			lager:info("Update news"),
			Body = update_db(Req),
			case Body of 			
				{fail, Error} ->
					lager:error("Error update news, reason : ", [Error]),
					error_response(Error, Req, State);
				_ ->
					success_response(Body, Req, State)
			end;
		
		_ ->
			lager:error("Error method not supported"),
			error_response(<<"Method not supported.">>, Req, State)
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% Get news
get_from_db(Req) ->
	Id = get_id(Req),
	case  Id of
		_ when Id > 0 ->
			 Resp = news_server:get_news(Id),		
			 convert_response(Resp);
		_ ->
			 Resp = news_server:get_all_news(),
			 convert_response(Resp)
	end.

convert_response(Resp)->
	case Resp of
		{atomic, Result} ->
			Responce = generate_news(Result),		
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
			mnesia_news:create_news(Escaped)
	end.
	
%% Update news
update_db(Req) ->
	case Id = get_id(Req) of
		_ when Id > 0 ->
			Content = get_content(Req),
			case validate(Content) of
				{fail, Error} ->
					{fail, Error};
				_ ->
					Escaped = escape_html_chars(Content),
					mnesia_news:update_news(Id, Escaped)
			end;
		_ -> 
			{fail,<<"Empty news_id path variable.">>}
	end.

%% Validate html
validate(Content) ->
	case validate_html:validate_html(Content) of
		false -> 
			lager:error("Error validete html"),
			{fail, <<"Not valid html.">>};
		true -> true
	end.

%% Success response
success_response(Result, Req, State) ->
	Body = result_to_json(Result),
	cowboy_req:set_resp_body(Body, Req),
	{true, Req, State}.

%% Error response
error_response(Error, Req, State) ->
	EncodedError = jsx:encode([{<<"error">>, Error}]),
	cowboy_req:reply(400, #{}, EncodedError, Req),
	{false, Req, State}.

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
	erlang:binary_to_integer(Number).

resource_exists(Req, State) ->
	case cowboy_req:binding(paste_id, Req, <<"0">>) of
		PasteID ->
			case valid_path(PasteID) of
				false -> 
					error_response(<<"Wrong news_id path variable.">>, Req, State);
				 _ -> {true, Req, PasteID}
			end
	end.

valid_path(Number) -> 
	try erlang:binary_to_integer(Number) 	
		catch
		    error:badarg -> 
				lager:error("Error erlang:binary_to_integer : ", Number),
				false
		end.

%% Convert db data for rest representation (updated)
generate_news(T) ->
	[[{<<"id">>, Id}, {<<"content">>, Content}] || {_, Id, Content} <- T].
	
%% Convert db data for rest representation (depricated)
convert_news(Resp) -> convert_news(Resp, []).
convert_news({atomic,[]},[]) -> [];
convert_news([], Resp) -> lists:reverse(Resp);
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