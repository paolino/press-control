# Naive machine tooling control

## Test

you need `cabal` executable . Use `ghcup`.


``` shell
# install haskell tooling
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
cabal update

# run the test suite
cabal test
```

## Done
- All library code
- Tests up-to `Operations` planning

## TODO
- CLI
- tests for `Machine` and `Load` module