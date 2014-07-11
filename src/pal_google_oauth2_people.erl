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

-module(pal_google_oauth2_people).
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
-define(INFO_URI, <<"https://www.googleapis.com/plus/v1/people/me">>).
-define(ACCESS_TOKEN, <<"access_token">>).
-define(ID, <<"id">>).
-define(URL, <<"url">>).
-define(URLS, <<"urls">>).
-define(LABEL, <<"label">>).
-define(VALUE, <<"value">>).
-define(TYPE, <<"type">>).
-define(ACCOUNT, <<"account">>).
-define(NAME, <<"name">>).
-define(DISPLAY_NAME, <<"displayName">>).
-define(FAMILY_NAME, <<"familyName">>).
-define(GINVEN_NAME, <<"givenName">>).
-define(IMAGE, <<"image">>).
-define(EMAIL, <<"email">>).
-define(EMAILS, <<"emails">>).
-define(VERIFIED_EMAIL, <<"verified_email">>).
-define(GOOGLE, <<"google">>).

%% Types
-type input()    :: #{credentials => #{access_token => binary()}}.
-type workflow() :: pal_authentication:workflow().

-record(state, {
	req_opts :: list()
}).

%% ==================================================================
%% Authentication callbacks
%% ==================================================================

-spec init(pal_workflow:options()) -> pal_workflow:handler(workflow()).
init(Opts) ->
	State = #state{req_opts = pt_mlist:get(request_options, Opts, [{follow_redirect, true}])},
	pal_authentication:init({{?MODULE, State}, Opts}).

-spec authenticate(input(), Req, workflow()) -> {pal_workflow:response(), Req} when Req :: cowboy_req:req().
authenticate(#{credentials := #{access_token := Token}}, Req, W) ->
	Uri = <<?INFO_URI/binary, $?, ?ACCESS_TOKEN/binary, $=, Token/binary>>,
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
				Message = <<"Google People request failed.">>,
				error_logger:error_report([{message, Message}, {reason, Reason}]),
				{fail, Message}
		end,
	
	{Resp, Req}.

-spec uid(workflow()) -> binary().
uid(W) ->
	pt_map:get(?ID, pal_authentication:raw_info(W)).

-spec info(workflow()) -> map().
info(W) ->
	RawInfo = pal_authentication:raw_info(W),
	Put =
		fun
			(_, undefined, M) -> M;
			(Key, Val, M)     -> maps:put(Key, Val, M)
		end,

	Urls =
		begin
			Urls1 =
				case pt_map:find(?URLS, RawInfo) of
					undefined ->
						#{};
					_ ->
						maps:from_list(
							lists:map(
								fun(M) ->
									{maps:get(?LABEL, M), maps:get(?VALUE, M)}
								end,
								maps:get(?URLS, RawInfo)))
				end,
			
			Urls2 =
				case pt_map:find(?URL, RawInfo) of
					undefined ->
						Urls1;
					Val ->
						Urls1#{?GOOGLE => Val}
				end,

			case pt_map:is_empty(Urls2) of
				true -> undefined;
				_    -> Urls2
			end
		end,
	
	[EMail|_] = [Val || #{?VALUE := Val, ?TYPE := ?ACCOUNT} <- pt_map:get(?EMAILS, RawInfo)],

	Put(name, pt_map:get(?DISPLAY_NAME, RawInfo),
		Put(first_name, pt_map:get_in([?NAME, ?GINVEN_NAME], RawInfo),
			Put(last_name, pt_map:get_in([?NAME, ?FAMILY_NAME], RawInfo),
				Put(image, pt_map:get_in([?IMAGE, ?URL], RawInfo),
					Put(email, EMail,
						Put(urls, Urls, #{})))))).

-spec extra(workflow()) -> map().
extra(W) ->
	#{raw_info => pal_authentication:raw_info(W)}.

