build:
    dune build

test stage grammar: build
    python ./test/run_tests.py --stage {{stage}} --grammar {{grammar}}

testl1:
    just test lexer 1

watch:
    dune build -w

run *args: build
    ./_build/default/src/driver/main.exe {{args}}
