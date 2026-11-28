# Revision history for roon-api

## 0.1.0.0 -- 2026-11-28

* First release on Hackage.
* MOO messages (`Roon.Moo.Message`), and requests and subscriptions over
  one WebSocket with keep-alive (`Roon.Effect.Moo`, `Roon.Effect.WebSocket`).
* Discovery of the Roon Servers of the local network (`Roon.Discovery`).
* Registration of an extension, its token kept from one run to the next
  (`Roon.Registry`, `Roon.Effect.TokenStore`, `Roon.Connection`).
* The transport service (`Roon.Transport`): zones and outputs, their
  changes, the queue, playback, seek, settings, volume and mute.
* The browse service (`Roon.Browse`): sessions, lists, the search for
  tracks, and the actions Roon offers on tracks, albums and playlists.
* Images over HTTP (`Roon.Image`).
* `roon-probe`, a program to try the library against a server.
