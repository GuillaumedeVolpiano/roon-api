# roon-api

A Haskell client library for the Roon extension API, built on
[Effectful](https://hackage.haskell.org/package/effectful) and
[Streamly](https://hackage.haskell.org/package/streamly-core).

This is an independent, unofficial implementation. It is not affiliated
with or endorsed by Roon Labs. The protocol was learnt from the public
reference implementation,
[node-roon-api](https://github.com/RoonLabs/node-roon-api); no code was
taken from it.

## Status

Early work in progress. Nothing here is stable yet.

- [x] MOO message encoding and parsing
- [x] WebSocket transport with keep-alive
- [x] Extension registration and token persistence
- [x] Server discovery (SOOD)
- [x] Transport service (zones, queue, playback control)
- [x] Browse service (search, library navigation, play actions)

## Building

    cabal build all
    cabal test

roon-api depends on
[effectful-streamly](https://github.com/GuillaumedeVolpiano/effectful-streamly)
and
[effectful-websockets](https://github.com/GuillaumedeVolpiano/effectful-websockets),
not yet on Hackage. `cabal.project` picks them up when they are checked out
next to this repository.

## Trying it against a server

    cabal run roon-probe -- [HOST [PORT]]

`roon-probe` connects to a Roon Server (by default `127.0.0.1`, port
`9330`), registers as an extension, prints what the server answers and lists
the zones.
The first time, it waits until you enable "roon-api probe" in Roon,
under Settings, Extensions. The token the server hands out is kept in
`$XDG_STATE_HOME/roon-api/`. With `ROON_TRACE=1`, it describes every
frame sent and received on stderr. `roon-probe discover` lists the Roon
Servers that answer on the local network.

Tested with GHC 9.12.4.

## License

BSD-3-Clause. See [LICENSE](LICENSE).
