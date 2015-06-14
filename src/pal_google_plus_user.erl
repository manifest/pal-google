%% ----------------------------------------------------------------------------
%% The MIT License
%%
%% Copyright (c) 2014-2015 Andrei Nesterov <ae.nesterov@gmail.com>
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
%% ----------------------------------------------------------------------------

-module(pal_google_plus_user).
-behaviour(pal_authentication).
-behaviour(pal_workflow).

%% Workflow callbacks
-export([
	decl/0
]).

%% Authentication callbacks
-export([
	authenticate/4,
	uid/1,
	info/2,
	extra/2
]).

%% Definitions
-define(GOOGLE_PLUS_API_URI, <<"https://www.googleapis.com/plus/v1">>).

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
-define(GENDER, <<"gender">>).
-define(IMAGE, <<"image">>).
-define(EMAIL, <<"email">>).
-define(EMAILS, <<"emails">>).

%% Types
-type data() :: #{access_token => binary()}.

%% ============================================================================
%% Workflow callbacks
%% ============================================================================

-spec decl() -> pal_workflow:declaration().
decl() ->
	Opts =
		#{request_options => [{follow_redirect, true}]},

	{pal_authentication, ?MODULE, Opts}.

%% ============================================================================
%% Authentication callbacks
%% ============================================================================

-spec authenticate(list(module()), data(), map(), map()) -> pal_authentication:result().
authenticate(Hs, #{access_token := Token} = Data, Meta, State) ->
	#{request_options := ReqOpts} = State,

	Uri =
		<<?GOOGLE_PLUS_API_URI/binary, "/people/me",
				$?, ?ACCESS_TOKEN/binary, $=, Token/binary>>,

	case hackney:get(Uri, [], <<>>, ReqOpts) of
		{ok, 200, _, Ref} ->
			{ok, Body} = hackney:body(Ref),
			{ok, jsx:decode(Body)};
		{ok, _, _, Ref} ->
			{ok, Body} = hackney:body(Ref),
			{error, {google_plus, Body}};
		{error, Reason} ->
			exit({Reason, {?MODULE, authenticate, [Hs, Data, Meta, State]}})
	end.

-spec uid(pal_authentication:rawdata()) -> binary().
uid(Data) ->
	pt_kvlist:get(?ID, Data).

-spec info(pal_authentication:rawdata(), map()) -> map().
info([{?DISPLAY_NAME, Val}|T], M) -> info(T, M#{name => Val});
info([{?NAME, Val}|T], M)         -> info(T, name(Val, M));
info([{?GENDER, Val}|T], M)       -> info(T, M#{gender => Val});
info([{?EMAILS, Val}|T], M)       -> info(T, account_email(Val, M));
info([{?IMAGE, Val}|T], M)        -> info(T, image(Val, M));
info([{?URL, Val}|T], M)          -> info(T, M#{uri => Val});
info([_|T], M)                    -> info(T, M);
info([], M)                       -> M.

-spec extra(pal_authentication:rawdata(), map()) -> map().
extra([{?URLS, Val}|T], M)   -> extra(T, uris(Val, M));
extra([{?EMAILS, Val}|T], M) -> extra(T, emails(Val, M));
extra([_|T], M)              -> extra(T, M);
extra([], M)                 -> M.

%% ============================================================================
%% Internal functions
%% ============================================================================

-spec name(pal_authentication:rawdata(), map()) -> map().
name([{?GINVEN_NAME, Val}|T], M) -> name(T, M#{first_name => Val});
name([{?FAMILY_NAME, Val}|T], M) -> name(T, M#{last_name => Val});
name([_|T], M)                   -> name(T, M);
name([], M)                      -> M.

-spec image(pal_authentication:rawdata(), map()) -> map().
image([{?URL, Val}|T], M) -> image(T, M#{image => Val});
image([_|T], M)           -> image(T, M);
image([], M)              -> M.

-spec account_email(list(pal_authentication:rawdata()), map()) -> map().
account_email(Emails, M) ->
	lists:foldl(
		fun(Email, Acc) ->
			case pt_kvlist:get(?TYPE, Email) of
				?ACCOUNT -> Acc#{email => pt_kvlist:get(?VALUE, Email)};
				_        -> Acc
			end
		end,
		M, Emails).

-spec emails(list(pal_authentication:rawdata()), map()) -> map().
emails(EMails, M) ->
	Val =
		lists:foldl(
			fun(EMail, Acc) ->
				case email(EMail, #{}) of
					#{type := ?ACCOUNT} -> Acc;
					M                   -> [M|Acc]
				end
			end, [], EMails),

	case length(Val) > 0 of
		true -> M#{emails => Val};
		_    -> M
	end.

-spec email(pal_authentication:rawdata(), map()) -> map().
email([{?TYPE, Val}|T], M)  -> email(T, M#{type => Val});
email([{?VALUE, Val}|T], M) -> email(T, M#{value => Val});
email([_|T], M)             -> email(T, M);
email([], M)								-> M.

-spec uris(list(pal_authentication:rawdata()), map()) -> map().
uris(Urls, M) ->
	Val =
		lists:map(
			fun(Url) -> uri(Url, #{}) end,
			Urls),

	M#{uris => Val}.

-spec uri(pal_authentication:rawdata(), map()) -> map().
uri([{?TYPE, Val}|T], M)  -> uri(T, M#{type => Val});
uri([{?LABEL, Val}|T], M) -> uri(T, M#{label => Val});
uri([{?VALUE, Val}|T], M) -> uri(T, M#{value => Val});
uri([_|T], M)             -> uri(T, M);
uri([], M)                -> M.

