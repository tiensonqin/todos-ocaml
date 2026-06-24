# Todos OCaml Web App

This is the web surface for the cross-platform todos app.

- `todos_web.ml` owns the web UI through `Bonsai_web` and `js_of_ocaml`.
- Todo state, actions, screen model, and command emission come from `Todos.Todo_core`.
- `todos_db_worker.ml` is compiled with `js_of_ocaml` and runs the same OCaml DataScript store logic as native.
- SQLite-wasm runs inside that worker as the persistence adapter, so reads and writes stay off the browser main thread.

Run:

```sh
dune build @web/build_web_static
python3 web/server.py
```
