# todos-ocaml

Native iOS todo app built with `datascript-ocaml` for domain state and
`bonsai-native` for UI rendering.

## What is included

- DataScript-backed todo store in `lib/todo_store.ml`.
- Focused model tests in `test/test_todo_store.ml`.
- Bonsai Native iOS entrypoint in `app/ios_app.ml`.
- Bundle id: `com.tiensonqin.todos`.
- Standard UIKit search chrome through `Apple.searchable`; on iOS 26 SDK builds,
  the system search controller receives the platform Liquid Glass treatment.

## Local dependencies

The opam file pins the GitHub repositories:

```sh
opam pin add -y datascript_ocaml git+https://github.com/logseq/datascript-ocaml.git
opam pin add -y bonsai_native git+https://github.com/logseq/bonsai-native.git
opam pin add -y bonsai_apple git+https://github.com/logseq/bonsai-native.git
opam install . --deps-only --with-test
```

## Test

```sh
dune runtest test
```

## Build for iOS simulator

Prepare the `simulator` switch using the Bonsai Native Apple build notes, then run:

```sh
IOS_TARGET=arm64-apple-ios17.0-simulator \
IOS_ARCH=arm64 \
IOS_SDKROOT=$(xcrun --sdk iphonesimulator --show-sdk-path) \
opam exec -- dune build app/Todos.app --workspace dune-workspace.simulator
```

For a physical device, prepare the `device` switch and build with
`--workspace dune-workspace.device`.
