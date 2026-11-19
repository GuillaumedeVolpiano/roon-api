# Revision history for roon-api

## 0.1.0.0 -- unreleased

* MOO message format: types, encoder and parser (`Roon.Moo.Message`).
* WebSocket transport with ping and pong (`Roon.Effect.WebSocket`).
* Requests and subscriptions multiplexed over one connection, and replies
  to the requests of the server (`Roon.Effect.Moo`).
* Extension registration (`Roon.Registry`) with token persistence
  (`Roon.Effect.TokenStore`).
* `withRoon` to connect and register in one step (`Roon.Connection`).
* `roon-probe`, a smoke test against a real server.
