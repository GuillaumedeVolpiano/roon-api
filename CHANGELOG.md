# Revision history for roon-api

## 0.1.0.0 -- unreleased

* MOO message format: types, encoder and parser (`Roon.Moo.Message`).
* WebSocket transport with ping and pong (`Roon.Effect.WebSocket`).
* Requests and subscriptions multiplexed over one connection, and replies
  to the requests of the server (`Roon.Effect.Moo`).
* Extension registration (`Roon.Registry`) with token persistence
  (`Roon.Effect.TokenStore`).
* `withRoon` to connect and register in one step (`Roon.Connection`).
* Transport service (`Roon.Transport`): zones and outputs, zone and queue
  subscriptions, playback, seek, settings, volume and mute controls, zone
  transfer. Views of the zones and of a queue kept up to date from their
  subscriptions.
* Building blocks shared by services, among them the subscription
  convention with its unsubscription (`Roon.Service`).
* `roon-probe`, a smoke test against a real server; it lists the zones.
