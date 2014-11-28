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

-module(pal_google_oauth2_authcode).
-behaviour(pal_oauth2_authcode).

%% OAuth2 AuthCode Workflow callbacks
-export([
	init/1,
	credentials/1
]).

% Definitions
-define(ACCESS_TOKEN, <<"access_token">>).
-define(ID_TOKEN, <<"id_token">>).
-define(TOKEN_TYPE, <<"token_type">>).
-define(EXPIRES_IN, <<"expires_in">>).

%% Types
-type handler(W) :: pal_workflow:handler(W).
-type options()  :: pal_workflow:options().
-type workflow() :: pal_oauth2_authcode:workflow().

%% ==================================================================
%% OAuth2 AuthCode Workflow callbacks
%% ==================================================================

-spec init(options()) -> handler(pal_oauth2_authcode:workflow()).
init(Opts) ->
	Opts2 =
		pt_kvterm:merge(
			#{authorization_uri => <<"https://accounts.google.com/o/oauth2/auth">>,
				access_token_uri  => <<"https://accounts.google.com/o/oauth2/token">>,
				scope             => [openid, email]},
			Opts),

	pal_oauth2_authcode:init({{?MODULE, undefined}, Opts2}).

-spec credentials(workflow()) -> #{access_token => binary()}.
credentials(W) ->
	RawInfo = pal_oauth2_authcode:raw_info(W),
	#{access_token => pt_map:get(?ACCESS_TOKEN, RawInfo),
		expires_in   => pt_map:find(?EXPIRES_IN, RawInfo),
		token_type   => pt_map:find(?TOKEN_TYPE, RawInfo),
		id_token     => pt_map:find(?ID_TOKEN, RawInfo)}.

