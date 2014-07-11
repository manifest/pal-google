# Pragmatic Authentication Library: Google OAuth2 workflows

collection of Google OAuth2 workflows for PAL

### 1. Google Login (OAuth2 Authorization Code Grant) workflow

For details, read the Google [documentation][google-authcode].

#### Options

You can configure several options, which you pass in to `pal:new/2` or `pal:init/1` functions via map:

- `client_id` (required) -
		The client ID that you obtain from the [Developers Console][google-developer-console],
		as described in [Obtain OAuth 2.0 credentials][google-obtain-credentials].
- `client_secret` (required) -
		The client secret that you obtain from the [Developers Console][google-developer-console],
		as described in [Obtain OAuth 2.0 credentials][google-obtain-credentials].
- `redirect_uri` (required) -
		The URI that you specify in the [Developers Console][google-developer-console],
		as described in [Set a redirect URI][google-redirect-uri].
- `scope` (optional) -
		which in a basic request should be `[openid, email]`.
		Read more at [scope][google-scope].
- `session` (recommended) -
		A session module which implements the `pt_session` behavior. For instanse, `pt_cowboy_session`.
		Used for holding a state between the request and callback.
- `request_options` (optional) -
		Request options for [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication map to be processed by the workflow.
		All by default, `[credentials, info, extra, rules]`.

#### Auth Map

Here's an example of an authentication map available in the HTTP handler
after success execution of `pal:authenticate/2` function.

```erlang
#{credentials =>
  #{access_token => <<"token-value">>,
    expires_in   => 3600,
    id_token     => <<"jwt-token-value">>,
    token_type   => <<"Bearer">>}}
```

#### How to use

```erlang
W = pal:new([pal_google_oauth2_authcode], Options),
pal:authenticate(W, Req).
```

For more details, see [pal-example][pal-example] project.

### 2. Google TokenInfo (Validating an ID token) workflow

#### Options

You can configure several options, which you pass in to `pal:new/2` or `pal:init/1` functions via map:

- `request_options` (optional) -
		Request options for [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication map to be processed by the workflow.
		All by default, `[credentials, info, extra, rules]`.

#### Input

- `id_token` (required) -
		ID Token obtained by `pal_google_oauth2_authcode` workflow.

```erlang
#{credentials =>
  #{id_token => <<"jwt-token-value">>}}
```

#### Auth Map

Here's an example of an authentication map available in the HTTP handler
after success execution of `pal:authenticate/2` function.

```erlang
#{credentials =>
  #{access_token => <<"token-value">>,
    expires_in   => 3600,
    id_token     => <<"jwt-token-value">>,
    token_type   => <<"Bearer">>},
  info =>
	#{email => <<"user@example.com">>},
    uid   => <<"1234567890">>}
```

#### How to use

```erlang
W = pal:new([pal_google_oauth2_authcode, pal_google_oauth2_tokeninfo], Options),
pal:authenticate(W, Req).
```

For more details, see [pal-example][pal-example] project.

### 3. Google+ People (Obtaining user profile information) workflow

#### Options

You can configure several options, which you pass in to `pal:new/2` or `pal:init/1` functions via map:

- `request_options` (optional) -
		Request options for [hackney][hackney] HTTP client.
- `includes` (optional) -
		Parts of authentication map to be processed by the workflow.
		All by default, `[credentials, info, extra, rules]`.

#### Input

- `access_token` (required) -
		OAuth2 Access Token obtained by `pal_google_oauth2_authcode` workflow.

```erlang
#{credentials =>
    #{access_token => <<"token-value">>}}
```

#### Auth Map

Here's an example of an authentication map available in the HTTP handler
after success execution of `pal:authenticate/2` function.

```erlang
#{credentials =>
  #{access_token => <<"token-value">>,
    expires_in   => 3600,
    id_token     => <<"jwt-token-value">>,
    token_type   => <<"Bearer">>},
  info =>
  #{email      => <<"user@example.com">>,
    first_name => <<"John">>,
    image      => <<"https://lh4.googleusercontent.com/url/photo.jpg?sz=50">>,
    last_name  => <<"Doe">>,
    name       => <<"John Doe">>,
    urls =>
    #{<<"google">> => <<"https://plus.google.com/+JohnDoe">>}},
  uid => <<"1234567890">>}
```

#### How to use

```erlang
W = pal:new([pal_google_oauth2_authcode, pal_google_oauth2_people], Options),
pal:authenticate(W, Req).
```

For more details, see [pal-example][pal-example] project.

### Documentation

See [pal][pal] and [pt-cowboy-session][pt-cowboy-session] projects for more information.

### License

Provided under the terms of [the MIT license][license].

[license]:http://www.opensource.org/licenses/MIT
[google-authcode]:https://developers.google.com/accounts/docs/OAuth2Login
[google-developer-console]:https://console.developers.google.com
[google-obtain-credentials]:https://developers.google.com/accounts/docs/OAuth2Login#getcredentials
[google-redirect-uri]:https://developers.google.com/accounts/docs/OAuth2Login#setredirecturi
[google-scope]:https://developers.google.com/+/api/oauth#login-scopes
[hackney]:https://github.com/benoitc/hackney
[pt-cowboy-session]:https://github.com/manifest/pt-cowboy-session
[pal]:https://github.com/manifest/pal
[pal-example]:https://github.com/manifest/pal-example

