# Pragmatic Authentication Library: Google workflows

Collection of Google workflows for [PAL][pal].

### 1. Google Login (OAuth2 Authorization Code Grant) workflow

For details, read the Google [documentation][google-oauth2-authcode].

#### Options

You can configure the workflow by passing options below into `pal:new/2` or `pal:group/2` functions:

- `client_id` (required) -
		The client ID obtained from the [Developers Console][google-developer-console].
- `client_secret` (required) -
		The client secret obtained from the [Developers Console][google-developer-console].
- `redirect_uri` (required) -
		The client redirection endpoint.
		After completing its interaction with the resource owner,
		the authorization server directs the resource owner's user-agent to this uri.
- `scope` (optional) -
		The [scope][google-oauth2-scope] of the access request.
- `request_options` (optional) -
		Options for the [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication schema to be processed by the workflow.
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

An successful execution of `pal:authenticate/{2,3}` function returns
the authentication schema below.

```erlang
#{access_token => <<"...">>, 
  token_type => <<"Bearer">>,
  expires_in => 3599,
  id_token => <<"...">>}
```

See a complete example with PAL and [Cowboy][cowboy] HTTP server [here][pal-example].

### 2. Google OpenID User (an ID token validation) workflow

#### Options

You can configure the workflow by passing options below into `pal:new/2` or `pal:group/2` functions:

- `request_options` (optional) -
		Options for the [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication schema to be processed by the workflow.
		All by default, `[uid, credentials, info, extra, rules]`.

#### Input Data

- `id_token` (required) -
		An id token obtained using the `pal_google_oauth2_authcode` workflow.

#### Authentication Schema

An successful execution of `pal:authenticate/{2,3}` function returns
the authentication schema below.

```erlang
#{uid => <<"...">>,
  info =>
    #{email => <<"john@example.com">>}}
```

See a complete example with PAL and [Cowboy][cowboy] HTTP server [here][pal-example].

### 3. Google+ User (user's profile data) workflow

#### Options

You can configure the workflow by passing options below into `pal:new/2` or `pal:group/2` functions:

- `request_options` (optional) -
		Options for the [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication schema to be processed by the workflow.
		All by default, `[uid, credentials, info, extra, rules]`.

#### Input Data

- `access_token` (required) -
		An access token obtained using the `pal_google_oauth2_authcode` workflow.

#### Authentication Schema

An successful execution of `pal:authenticate/{2,3}` function returns
the authentication schema below.

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

See a complete example with PAL and [Cowboy][cowboy] HTTP server [here][pal-example].

### License

The source code is provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[cowboy]:https://github.com/extend/cowboy
[google-oauth2-authcode]:https://developers.google.com/accounts/docs/OAuth2Login
[google-oauth2-scope]:https://developers.google.com/+/api/oauth#login-scopes
[google-developer-console]:https://console.developers.google.com
[google-redirect-uri]:https://developers.google.com/accounts/docs/OAuth2Login#setredirecturi
[hackney]:https://github.com/benoitc/hackney
[pal]:https://github.com/manifest/pal
[pal-example]:https://github.com/manifest/pal-example

