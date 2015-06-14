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

-module(pal_google_oauth2_tokeninfo).
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
-define(GOOGLE_OPENID_API_URI, <<"https://www.googleapis.com/oauth2/v1">>).

-define(ID_TOKEN, <<"id_token">>).
-define(USER_ID, <<"user_id">>).
-define(EMAIL, <<"email">>).
-define(EMAIL_VERIFIED, <<"email_verified">>).

%% Types
-type data() :: #{id_token => binary()}.

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
authenticate(Hs, #{id_token := Token} = Data, Meta, State) ->
	#{request_options := ReqOpts} = State,

	Uri =
		<<?GOOGLE_OPENID_API_URI/binary, "/tokeninfo",
				$?, ?ID_TOKEN/binary, $=, Token/binary>>,

	case hackney:get(Uri, [], <<>>, ReqOpts) of
		{ok, 200, _, Ref} ->
			{ok, Body} = hackney:body(Ref),
			{ok, jsx:decode(Body)};
		{ok, _, _, Ref} ->
			{ok, Body} = hackney:body(Ref),
			{error, {google_oauth2, jsx:decode(Body)}};
		{error, Reason} ->
			exit({Reason, {?MODULE, authenticate, [Hs, Data, Meta, State]}})
	end.

-spec uid(pal_authentication:rawdata()) -> binary().
uid(Data) ->
	pt_kvlist:get(?USER_ID, Data).

-spec info(pal_authentication:rawdata(), map()) -> map().
info(Data, M) ->
	case pt_kvlist:find(?EMAIL_VERIFIED, Data) of
		{ok, true} -> M#{email => pt_kvlist:get(?EMAIL, Data)};
		_          -> M
	end.

