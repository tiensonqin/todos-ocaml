# Todos OCaml Web App

This is the web surface for the cross-platform todos app.

- `todos_web.ml` owns the web UI through Melange and React.
- UI state and actions live in OCaml and render directly through React.
- Todo persistence goes through a web worker backed by SQLite wasm and Transit
  JSON codecs.

Run:

```sh
opam exec -- dune build @web-demo
cd web
npm install
npm run dev
```
