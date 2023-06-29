# nrte

An OTP application to send and receive real time events over HTTP connections.

## Status

![GitHub branch checks state](https://img.shields.io/github/checks-status/nomasystems/nrte/main)
![GitHub Workflow Status (branch)](https://img.shields.io/github/workflow/status/nomasystems/nrte/ci/main)
![Coveralls branch](https://img.shields.io/coveralls/github/nomasystems/nrte/main)

## Prerequisites

![Min. OTP version](https://img.shields.io/badge/min._OTP-26-blue)
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
    webSocket.send("authorization: user:pass");
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

## Bypassing the HTTP connections

All events are published and received through the `[erlbus](https://github.com/cabol/erlbus)` library. It is possible to directly use it along the HTTP interfaces by publishing or subscribing to the same topics.

## Support

Any doubt or suggestion? Please, read the [documentation](http://nomasystems.github.io/nrte) and check out [our issue tracker](https://github.com/nomasystems/nrte/issues).

## Contributing

Pull requests are welcome. Please read the [contributing guidelines](CONTRIBUTING.md) to know more about contribution.
