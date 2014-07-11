%% ------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2014 Andrei Nesterov <ae.nesterov@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to
%% deal in the Software without restriction, including without limitation the
%% rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
%% sell copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.
%% ------------------------------------------------------------------

-module(pal_google_oauth2_tokeninfo).
-behaviour(pal_authentication).

%% Authentication callbacks
-export([
	init/1,
	authenticate/3,
	uid/1,
	info/1,
	extra/1
]).

%% Definitions
-define(INFO_URI, <<"https://www.googleapis.com/oauth2/v1/tokeninfo">>).
-define(ID_TOKEN, <<"id_token">>).
-define(USER_ID, <<"user_id">>).
-define(EMAIL, <<"email">>).
-define(VERIFIED_EMAIL, <<"verified_email">>).

%% Types
-type input()    :: #{credentials => #{id_token => binary()}}.
-type workflow() :: pal_authentication:workflow().

-record(state, {
	req_opts :: undefined | list()
}).

%% ==================================================================
%% Authentication callbacks
%% ==================================================================

-spec init(pal_workflow:options()) -> pal_workflow:handler(workflow()).
init(Opts) ->
	State = #state{req_opts = pt_mlist:get(request_options, Opts, [{follow_redirect, true}])},
	pal_authentication:init({{?MODULE, State}, Opts}).

-spec authenticate(input(), Req, workflow()) -> {pal_workflow:response(), Req} when Req :: cowboy_req:req().
authenticate(#{credentials := #{id_token := Token}}, Req, W) ->
	Uri = <<?INFO_URI/binary, $?, ?ID_TOKEN/binary, $=, Token/binary>>,
	State = pal_authentication:handler_state(W),
	Resp =
		case hackney:get(Uri, [], <<>>, State#state.req_opts) of
			{ok, 200, _, Ref} ->
				pal_oauth2:from_json(Ref, fun(M) ->
					M
				end);
			{ok, _, _, Ref} ->
				pal_oauth2:from_json(Ref, fun(M) ->
					{fail, M}
				end);
			{error, Reason} ->
				Message = <<"Google TokenInfo request failed.">>,
				error_logger:error_report([{message, Message}, {reason, Reason}]),
				{fail, Message}
		end,
	
	{Resp, Req}.

-spec uid(workflow()) -> binary().
uid(W) ->
	pt_map:get(?USER_ID, pal_authentication:raw_info(W)).

-spec info(workflow()) -> map().
info(W) ->
	RawInfo = pal_authentication:raw_info(W),
	case pt_map:get(?VERIFIED_EMAIL, RawInfo, false) of
		true ->
			#{email => pt_map:find(?EMAIL, RawInfo)};
		false ->
			#{}
	end.

-spec extra(workflow()) -> map().
extra(W) ->
	#{raw_info => pal_authentication:raw_info(W)}.

