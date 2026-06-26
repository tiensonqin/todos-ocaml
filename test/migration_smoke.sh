#!/bin/sh
set -eu

script_dir=$(CDPATH= cd -- "$(dirname "$0")" && pwd)
repo_root=$(CDPATH= cd -- "$script_dir/.." && pwd)
cd "$repo_root"

if grep -R "bonsai_web\\|js_of_ocaml\\|bonsai.ppx_bonsai" dune-project todos_ocaml.opam lib/dune web/dune >/dev/null; then
  echo "Dune/opam config should not depend on Bonsai Web, js_of_ocaml, or Bonsai ppx" >&2
  exit 1
fi

if grep -ER 'ppx_jane|core_kernel|"core"|\(core([[:space:]]|\))|^[[:space:]]*core([[:space:]]|\))' dune-project todos_ocaml.opam lib/dune app/dune test/dune >/dev/null; then
  echo "Dune/opam config should not depend on ppx_jane or Core" >&2
  exit 1
fi

test -s web/dist/web/todos_web.js
test -s web/dist/web/react_runtime.js

grep -q "createRenderer" web/dist/web/todos_web.js
grep -q "New task" web/dist/web/todos_web.js
grep -q "createRoot" web/dist/web/react_runtime.js
grep -q "Todos" web/dist/web/react_runtime.js
