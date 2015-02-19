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

-module(pal_google_oauth2_people).
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
	info/2
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
authenticate(_, #{access_token := Token}, _, #{request_options := ReqOpts}) ->
	Uri = <<?INFO_URI/binary, $?, ?ACCESS_TOKEN/binary, $=, Token/binary>>,
	case hackney:get(Uri, [], <<>>, ReqOpts) of
		{ok, 200, _, Ref} ->
			{ok, Body} = hackney:body(Ref),
			{ok, jsx:decode(Body)};
		{ok, _, _, Ref} ->
			{ok, Body} = hackney:body(Ref),
			{error, {google_plus, jsx:decode(Body)}};
		{error, Reason} ->
			throw({bad_req, Reason})
	end.

-spec uid(pal_authentication:rawdata()) -> binary().
uid(Data) ->
	pt_kvlist:get(?ID, Data).

-spec info(pal_authentication:rawdata(), map()) -> map().
info([{?DISPLAY_NAME, Val}|T], M) -> info(T, M#{name => Val});
info([{?NAME, Val}|T], M)         -> info(T, name(Val, M));
info([{?IMAGE, Val}|T], M)        -> info(T, image(Val, M));
info([{?URLS, Val}|T], M)         -> info(T, M#{urls => urls(Val, maps:get(urls, M, #{}))});
info([{?URL, Val}|T], M)          -> info(T, M#{urls => maps:put(?GOOGLE, Val, maps:get(urls, M, #{}))});
info([_|T], M)                    -> info(T, M);
info([], M)                       -> M.

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

-spec urls(list(pal_authentication:rawdata()), map()) -> map().
urls([Val|T], M) -> urls(T, url(Val, M));
urls([], M)      -> M.

-spec url(pal_authentication:rawdata(), map()) -> map().
url(L, M) ->
	maps:put(
		pt_kvlist:get(?LABEL, L),
		pt_kvlist:get(?VALUE, L),
		M).

