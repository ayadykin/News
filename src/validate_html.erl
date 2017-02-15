%% @author Andrei
%% @doc @todo Add description to validate_html.

-module(validate_html).

-include_lib("xmerl/include/xmerl.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([validate_html/1]).

validate_html(Content)->
	try 
		case xmerl_scan:string(erlang:binary_to_list(Content)) of
			{Xml, _} ->  
				[val(xmerl_xpath:string("//html", Xml))],
				[val(xmerl_xpath:string("//head", Xml))],
				[val(xmerl_xpath:string("//body", Xml))],
				true
		end
	catch
		error:{badmatch, _} -> false;
		exit:Exit -> false
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

val(X) ->
    [#xmlElement{name = N}] = X,
	{N}.
	

