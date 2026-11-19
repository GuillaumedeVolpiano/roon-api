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
- [ ] Server discovery (SOOD)
- [ ] Transport service (zones, queue, playback control)
- [ ] Browse service (search, library navigation, play actions)

## Building

    cabal build all
    cabal test

## Trying it against a server

    cabal run roon-probe -- [HOST [PORT]]

`roon-probe` connects to a Roon Server (by default `127.0.0.1`, port
`9330`), registers as an extension and prints what the server answers.
The first time, it waits until you enable "roon-api probe" in Roon,
under Settings, Extensions. The token the server hands out is kept in
`$XDG_STATE_HOME/roon-api/`.

Tested with GHC 9.12.4.

## License

BSD-3-Clause. See [LICENSE](LICENSE).
