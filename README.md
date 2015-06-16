# Pragmatic Authentication Library: Google workflows

Collection of Google workflows for [PAL][pal].

### 1. Google Login (OAuth2 Authorization Code Grant) workflow

For details, read the Google [documentation][google-authcode].

#### Options

You can configure the workflow, passing the options into `pal:new/2` or `pal:group/2` functions:

- `client_id` (required) -
		The client ID was obtained from the [Developers Console][google-developer-console],
		as described in [Obtain OAuth 2.0 credentials][google-obtain-credentials].
- `client_secret` (required) -
		The client secret was obtained from the [Developers Console][google-developer-console],
		as described in [Obtain OAuth 2.0 credentials][google-obtain-credentials].
- `redirect_uri` (required) -
		The callback endpoint [Developers Console][google-developer-console],
		as described in [Set a redirect URI][google-redirect-uri].
- `scope` (optional) -
		which in a basic request should be `[openid, email]`.
		Read [here][google-scope] more about Google scopes.
- `request_options` (optional) -
		Request options, in the format of [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication map to be processed by the workflow.
		All by default, `[uid, credentials, info, extra, rules]`.

#### Input Data

- `code` -
		The authorization code.
- `state` -
		The state was previously passed to the authentication provider.
- `error`
		If the request fails due to a missing, invalid, or mismatching
		redirection URI, or if the client identifier is missing or invalid.

#### Authentication Schema

If an execution of the `pal:authenticate/{2,3}` function were successful,
the authentication schema would be returned:

```erlang
#{access_token => <<"...">>, 
  token_type => <<"Bearer">>,
  expires_in => 3599,
  id_token => <<"...">>}
```

See the complete example using PAL and [Cowboy][cowboy] HTTP server [here][pal-example].

### 2. Google OpenID User (an ID token validation) workflow

#### Options

You can configure the workflow, passing the options into `pal:new/2` or `pal:group/2` functions:

- `request_options` (optional) -
		Request options, in the format of [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication map to be processed by the workflow.
		All by default, `[uid, credentials, info, extra, rules]`.

#### Input Data

- `id_token` (required) -
		ID Token was obtained with the `pal_google_oauth2_authcode` workflow.

#### Authentication Schema

If an execution of the `pal:authenticate/{2,3}` function were successful,
the authentication schema would be returned:

```erlang
#{uid => <<"...">>,
  info =>
    #{email => <<"john@example.com">>}}
```

See the complete example using PAL and [Cowboy][cowboy] HTTP server [here][pal-example].

### 3. Google+ User (Obtaining user's profile data) workflow

#### Options

You can configure the workflow, passing the options into `pal:new/2` or `pal:group/2` functions:

- `request_options` (optional) -
		Request options, in the format of [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication map to be processed by the workflow.
		All by default, `[uid, credentials, info, extra, rules]`.

#### Input Data

- `access_token` (required) -
		OAuth2 Access Token was obtained with the `pal_google_oauth2_authcode` workflow.

#### Authentication Schema

If an execution of the `pal:authenticate/{2,3}` function were successful,
the authentication schema would be returned:

```erlang
#{uid => <<"...">>,
  info =>
    #{name => <<"John Doe">>,
      first_name => <<"John">>,
      last_name => <<"Doe">>,
      gender => <<"male">>,
      email => <<"john@example.com">>,
      image => <<"https://lh3.googleusercontent.com/...">>,
      uri => <<"https://plus.google.com/...">>}}
```

See the complete example using PAL and [Cowboy][cowboy] HTTP server [here][pal-example].

### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[cowboy]:https://github.com/extend/cowboy
[google-authcode]:https://developers.google.com/accounts/docs/OAuth2Login
[google-developer-console]:https://console.developers.google.com
[google-obtain-credentials]:https://developers.google.com/accounts/docs/OAuth2Login#getcredentials
[google-redirect-uri]:https://developers.google.com/accounts/docs/OAuth2Login#setredirecturi
[google-scope]:https://developers.google.com/+/api/oauth#login-scopes
[hackney]:https://github.com/benoitc/hackney
[pal]:https://github.com/manifest/pal
[pal-example]:https://github.com/manifest/pal-example

