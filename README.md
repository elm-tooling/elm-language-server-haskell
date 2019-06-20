# elm-language-server-haskell (unmaintained and archived)

**NOTE**: The repository [elm-languge-server](https://github.com/elm-tooling/elm-language-server) (tree-sitter based) is actively maintained. This repository is currently *not maintained*.

---

First attempt to write a language server for Elm.

See https://microsoft.github.io/language-server-protocol/specification for
a description of the protocol.

## Features
* Diagnostics

If you want to work on more, please reach out and create an issue, before you start working on it.

## Conceptual workings of the language server
The editor should start one language server for each Elm project (there may be multiple). `rootPath` should be set to the location of the `elm.json` file.
* On the initialize notification, the language server will copy all files in the project to a temporary directory. This is so that we can handle open files by saving them there. It will then compile all files in the project, to get diagnostics. It will then send diagnostics for all files to the editor.
* When a file is changed in the editor, we change it in our copy, and recompile everything to get new diagnostics.

## Libraries used
* haskell-lsp
* json-rpc-server
* elm-compiler-library (which is a version of elm-compiler)

## Notes
* Code formatted using `hlint`

## Building
Clone the repository and its subrepositories:
* `git clone https://github.com/elm-tooling/elm-language-server`
* `git submodule update --init --recursive`
Install ghc and dependencies. You need to have [stack](https://www.haskellstack.org) installed
* `stack setup`
* `stack install`

## Contributing
* [Elm Language Server - Requirements](https://docs.google.com/document/d/1ETeZeN17hqM4yui4iqBv1jwO8HryDZyel2kdcxTCGGA/edit)
* Get information about contributing on the [\#elm-language-server channel](https://elmlang.slack.com/messages/elm-language-server) in the elm slack.
