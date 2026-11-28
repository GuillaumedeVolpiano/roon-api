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

- [x] MOO message encoding and parsing
- [x] WebSocket transport with keep-alive
- [x] Extension registration and token persistence
- [x] Server discovery (SOOD)
- [x] Transport service (zones, queue, playback control)
- [x] Browse service (search, library navigation, play actions)
- [x] Images (over HTTP)

## Building

    cabal build all
    cabal test

To build against copies of the libraries this package is built on
([effectful-streamly](https://hackage.haskell.org/package/effectful-streamly),
[effectful-streamly-http](https://hackage.haskell.org/package/effectful-streamly-http),
[effectful-websockets](https://hackage.haskell.org/package/effectful-websockets),
[streamly-network-udp](https://hackage.haskell.org/package/streamly-network-udp)
and those under them) checked out next to this repository, pass
`--project-file=cabal.project.siblings`.

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
`roon-probe browse HIERARCHY [TITLE...]` walks the library by the titles of
its items and prints the level reached, without playing anything.
`roon-probe image KEY FILE [HOST [PORT]]` writes the image of an image key
(as found in the traced frames) to a file.

Tested with GHC 9.8.4, 9.10.3 and 9.12.4, and against Roon Server 2.75.

## License

BSD-3-Clause. See [LICENSE](LICENSE).
