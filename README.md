# nrte

An OTP application to send and receive real time events over HTTP connections.

## Status
[![GitHub branch checks state](https://github.com/nomasystems/nrte/actions/workflows/ci.yml/badge.svg)](https://github.com/nomasystems/nrte/actions/workflows/ci.yml)


## Prerequisites

![Min. OTP version](https://img.shields.io/badge/min._OTP-25.3.2-blue)
![Max. OTP version](https://img.shields.io/badge/max._OTP-26-blue)
![Min. rebar version](https://img.shields.io/badge/min._rebar-3.22.X-blue)

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
const webSocket = new WebSocket('ws://localhost:2080/websocket');
webSocket.onopen = function(event) {
    webSocket.send("topics: topic1;topic2;topic3");
};
```

Or via server-sent events in `curl` 
```sh
curl 'localhost:2080/eventsource?topics=topic1;topic2;topic3'
```

Then, all messages posted to the `/message` endpoint will be redirected to the online subscribers:
```sh
curl -X POST -d '{"topics": ["topic1", "topic2"], "message": "text"}' 'localhost:2080/message'
```

## Configuration

`nrte` defaults to the following configuration values:
```erl
[
  {auth_type, {always,true}},
  {data_template, {<<"{{topic}};{{message}}">>}},
  {port, 2080},
  {serve_priv_dir, false},
  {token_cleanup_seconds, 60},
  {token_expiration_seconds, 60}
]
```

* `auth_type`: see [authentication](#authentication) for details.
* `data_template`: a template for sending the data through the http connections. Both `{{topic}}` and `{{message}}` are optional and will be replaced with the actual values.
* `port`: TCP port that serves the different endpoints.
* `serve_priv_dir`: whether to include the [priv dir](/priv/) in the server or not.
* `token_cleanup_seconds`: how often should the expired tokens be deleted.
* `token_expiration_seconds`: how long is the authentication token valid.

## Authentication

By default nrte doesn't authenticate its users, but this can be changed by setting the `auth_type` configuration parameter to a tuple `{Mod, Fun}`. When a request arrives with an `Authorization` header, `nrte` will call `Mod:Fun(AuthorizationBinaryValue)` and use the returned `boolean()` to allow or deny access to the resource.

Since some WebSocket or EventSource interfaces don't allow setting authorization headers, there's also an `/auth` endpoint that will return a one-use authentication cookie that can be used in the next connection. See the [`JS tester`](priv/tester.html) for an implementation example.

## Bypassing the HTTP connections

All events are published and received through the [`erlbus`](https://github.com/cabol/erlbus) library. It is possible to directly use it along the HTTP interfaces by publishing or subscribing to the same topics.

## Support

Any doubt or suggestion? Please, check out [our issue tracker](https://github.com/nomasystems/nrte/issues).

## Contributing

Pull requests are welcome. Please read the [contributing guidelines](CONTRIBUTING.md) to know more about contribution.
