# elm-language-server

First attempt to write a language server for Elm.

See https://microsoft.github.io/language-server-protocol/specification for
a description of the protocol.

## Conceptual workings of the language server
The editor should start one language server for each Elm project (there may be multiple). `rootPath` should be set to the location of the `elm.json` file.
* On the initialize notification, the language server will copy all files in the project to a temporary directory. This is so that we can handle open files by saving them there. It will then compile all files in the project, to get diagnostics. It will then send diagnostics for all files to the editor.
* When a file is changed in the editor, we change it in our copy, and recompile everything to get new diagnostics.

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