build:
    dune build

test stage grammar: build
    python ./test/run_tests.py --stage {{stage}} --grammar {{grammar}}

testl1:
    just test lexer 1

watch:
    dune build -w

run *args: build
    dune exec src/driver/main.exe -- {{args}}

debug *args:
    dune build src/driver/main.bc
    ocamldebug _build/default/src/driver/main.bc {{args}}
