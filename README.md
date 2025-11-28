# nrte

An OTP application to send and receive real time events over HTTP connections.

## Status
[![GitHub branch checks state](https://github.com/nomasystems/nrte/actions/workflows/ci.yml/badge.svg)](https://github.com/nomasystems/nrte/actions/workflows/ci.yml)

## Prerequisites

![Min. OTP version](https://img.shields.io/badge/min._OTP-28-blue)
![Min. rebar version](https://img.shields.io/badge/min._rebar-3.25.X-blue)

## Usage

In your `rebar.config` file, add the dependency:
```erl
{deps, [
    {nrte, {git, "git@github.com:nomasystems/nrte.git", {branch, "main"}}}
]}.
```

Then, you can connect either via WebSocket or server-sent events to start receiving messages for the given topics.

For example, in javascript via the `WebSocket` interface:
```js
const webSocket = new WebSocket('ws://localhost:2080/websocket?topics=topic1;topic2;topic3');
```

Or via server-sent events in `curl`:
```sh
curl 'localhost:2080/eventsource?topics=topic1;topic2;topic3'
```

Then, all data posted to the `/message` endpoint will be redirected to the online subscribers:
```sh
curl -X POST -d 'my custom message' 'localhost:2080/message?topics=topic1'
```

## Configuration

`nrte` defaults to the following configuration values:
```erl
[
  {auth_type, {always_allow, all}},
  {data_template, {<<"{{topic}};{{message}}">>}},
  {port, 2080},
  {serve_priv_dir, false}
]
```

* `auth_type`: see [authentication](#authentication) for details.
* `data_template`: a template for sending the data through the http connections. Both `{{topic}}` and `{{message}}` are optional and will be replaced with the actual values.
* `port`: TCP port that serves the different endpoints.
* `serve_priv_dir`: whether to include the [priv dir](/priv/) in the server or not.

## Authentication

By default nrte doesn't authenticate its users, but this can be changed by setting the `auth_type` configuration parameter to a tuple `{auth_mod, Mod}`. In that case, when a request arrives, `nrte` will call `Mod:nrte_auth(Headers)` and use the returned `nrte_auth_value()` to allow or deny access to the resource. See the [`nrte_auth` module](/src/nrte_auth.erl) for the behaviour to implement.

Some examples:
```erl
% Wrong credentials; results in a 401
nrte_auth(_Headers) ->
  unauthorized.

% Disallow access to all topics; results in a 403
nrte_auth(_Headers) ->
  none.

% Allow access to all topics
nrte_auth(_Headers) ->
  all.

% Allow access to subscribe to some topics but disallow publishing
nrte_auth(_Headers) ->
  #{allowed_publications => none,
    allowed_subscriptions => ["allowed_topic_prefix.*", "other_topic"]}.
```

## Bypassing the HTTP connections

Messages can also be published via `nrte:publish/2` and subscribed from `nrte:subscribe/1`. In the latter case, the caller will receive messages in the form of `{nrte_message, Data}`:
```erl
> nrte:subscribe([<<"example">>]).
ok

> nrte:publish(<<"example">>, <<"my-message">>).
ok

> receive {nrte_message, Data} -> Data end.
<<"example;my-message">>
```

## Support

Any doubt or suggestion? Please, check out [our issue tracker](https://github.com/nomasystems/nrte/issues).

## Contributing

Pull requests are welcome. Please read the [contributing guidelines](CONTRIBUTING.md) to know more about contribution.
