# Revision history for roon-api

## 0.1.0.0 -- unreleased

* MOO message format: types, encoder and parser (`Roon.Moo.Message`).
* WebSocket transport with ping and pong (`Roon.Effect.WebSocket`), over
  `effectful-websockets`.
* Requests and subscriptions multiplexed over one connection, and replies
  to the requests of the server in a thread of their own
  (`Roon.Effect.Moo`).
* Time limits: 30 seconds per reply by default for requests
  (`requestWithin` to change it), none for registration unless asked
  (`registerWithin`).
* Extension registration (`Roon.Registry`) with token persistence
  (`Roon.Effect.TokenStore`).
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
* Building blocks shared by services, among them the subscription
  convention with its unsubscription (`Roon.Service`).
* `roon-probe`, a smoke test against a real server; it lists the zones.
