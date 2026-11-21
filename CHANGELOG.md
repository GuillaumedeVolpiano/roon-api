# Revision history for roon-api

## 0.2.0.0 -- unreleased

* `Roon.Image` asks for images through the `Http` effect of
  effectful-streamly-http: `fetchImage` no longer needs `IOE`, and tests
  answer from a function. `runImageHttp` reaches the server, within
  `ImageLimits`; `fetchImageWithin` is gone. The time limit now bounds each
  wait, not the whole exchange.
* `performAction` plays an album, a playlist or anything else Roon offers
  actions for, not only a track.
* `roon-probe browse HIERARCHY [TITLE...]` walks the library without
  playing anything.
* Built on streamly-http 0.2 and effectful-websockets 0.1.0.1.

## 0.1.0.0 -- 2026-11-21

* MOO message format: types, encoder and parser (`Roon.Moo.Message`).
* WebSocket transport with ping and pong (`Roon.Effect.WebSocket`), over
  `effectful-websockets`, and a trace of the MOO frames (`traceFrames`).
* Requests and subscriptions multiplexed over one connection, and replies
  to the requests of the server in a thread of their own
  (`Roon.Effect.Moo`).
* Time limits: 30 seconds per reply by default for requests
  (`requestWithin` to change it), none for registration unless asked
  (`registerWithin`).
* Extension registration (`Roon.Registry`) with token persistence
  (`Roon.Effect.TokenStore`).
* Discovery of Roon Servers on the local network, by SOOD
  (`Roon.Discovery`), over `streamly-network-udp`.
* `withRoon` to connect and register in one step, and
  `withRoonReconnecting` to connect again, with a growing delay, when the
  connection is lost or refused (`Roon.Connection`).
* Transport service (`Roon.Transport`): zones and outputs, zone and queue
  subscriptions, playback, seek, settings, volume and mute controls, zone
  transfer. Views of the zones and of a queue kept up to date from their
  subscriptions, with seek moves held back on request (`zoneViewsEvery`).
  Zones and queues followed as one stream (`subscribeTransport`).
* Browse service (`Roon.Browse`): browsing and loading in sessions of
  their own, track searches run concurrently with a report for each query,
  and the actions on a track (Play Now, Add Next, Queue, Start Radio).
* Images fetched over HTTP by their key, in a size and a format asked for,
  within limits of time and size (`Roon.Image`), over `streamly-http`.
* Building blocks shared by services, among them the subscription
  convention with its unsubscription (`Roon.Service`).
* `roon-probe`, a smoke test against a real server; it lists the zones, and
  describes every frame when `ROON_TRACE` is set; `roon-probe discover`
  lists the servers on the network; `roon-probe image` saves an image.
