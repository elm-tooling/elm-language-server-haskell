# elm-language-server

First attempt to write a language server for Elm.

See https://microsoft.github.io/language-server-protocol/specification for
a description of the protocol.

## Libraries used
* haskell-lsp
* json-rpc-server
* elm-compiler

## Notes
* Code formatted using `hlint`

## Building
`stack setup`
`stack build`
`stack exec elm-language-server-exe`