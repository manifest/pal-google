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

-module(pal_google_oauth2_authcode).
-behaviour(pal_workflow).

%% Workflow callbacks
-export([
	decl/0
]).

%% Authentication workflow callbacks
-export([
	credentials/2
]).

% Definitions
-define(ACCESS_TOKEN, <<"access_token">>).
-define(ID_TOKEN, <<"id_token">>).
-define(TOKEN_TYPE, <<"token_type">>).
-define(EXPIRES_IN, <<"expires_in">>).
-define(OPENID, <<"openid">>).
-define(EMAIL, <<"email">>).

%% ============================================================================
%% Workflow callbacks
%% ============================================================================

-spec decl() -> pal_workflow:declaration().
decl() ->
	Opts =
		#{authorization_uri => <<"https://accounts.google.com/o/oauth2/auth">>,
			access_token_uri  => <<"https://accounts.google.com/o/oauth2/token">>,
			scope             => [?OPENID, ?EMAIL]},

	{pal_oauth2_authcode, ?MODULE, Opts}.

%% ============================================================================
%% Authentication workflow callbacks
%% ============================================================================

-spec credentials(pal_authentication:rawdata(), map()) -> map().
credentials([{?ACCESS_TOKEN, Val}|T], M) -> credentials(T, M#{access_token => Val});
credentials([{?EXPIRES_IN, Val}|T], M)   -> credentials(T, M#{expires_in => Val});
credentials([{?TOKEN_TYPE, Val}|T], M)   -> credentials(T, M#{token_type => Val});
credentials([{?ID_TOKEN, Val}|T], M)     -> credentials(T, M#{id_token => Val});
credentials([_|T], M)                    -> credentials(T, M);
credentials([], M)                       -> M.

