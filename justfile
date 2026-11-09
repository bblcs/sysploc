build:
    dune build

test stage grammar: build
    python ./test/run_tests.py --stage {{stage}} --grammar {{grammar}}

watch:
    dune build -w

run *args: build
    dune exec src/driver/main.exe -- {{args}}

runall *args: build
    dune exec src/driver/main.exe -- -v -t /dev/null -a /dev/null -o /dev/null {{args}}

debug *args:
    dune build src/driver/main.bc
    ocamldebug _build/default/src/driver/main.bc {{args}}
