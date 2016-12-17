# lo vegadri

[![Build Status](https://travis-ci.org/MarkMcCaskey/lo-vegadri.svg?branch=master)](https://travis-ci.org/MarkMcCaskey/lo-vegadri)

WARNING: this project is in early development and is likely unsafe and broken.  Expect bugs.  Contributions welcome.

All current design and implementation details are subject to change.

## About

An implementation of a semantic Lojban standard (currently unnamed).

This project is unofficial and is not affiliated with the LLG.

[Lojban](https://mw.lojban.org/papri/Lojban) is a constructed language with many properties that are likely valuable for human-computer interaction.

### Motivation

The original design goal was to create a generic, flexible, and speakable interface to structured-data traversal in a manner similar to the [lens](https://hackage.haskell.org/package/lens) library.  As the project progressed, it's changed significantly and is now closer to a logic programming language.  (There are definitely similarities between the type of traversal done by Datalog and the types of traversals done by lens-like objects.  This is a topic worth exploring.)

As such, the project is currently in a state of active research and does not currently have a clear path to "completion".  As the system is made useful, such a path may become clear.  Current ideas and plans are kept in the [todo list](docs/TODO.org).

The [spec](docs/lojban-spec.org) is mostly focused on describing abstractly how and what semantic lojban systems (for computation but mostly for querying) should do things or include.  It's currently very under-specified in most places and probably a bit excessively so in others.  Please feel free to submit suggestions or make derivatives.

## Installation

You will need [stack](https://docs.haskellstack.org/en/stable/README/) to build this project.


To install the project:
```
git clone https://github.com/MarkMcCaskey/lo-vegadri --recursive
cd lo-vegadri
stack setup
stack install
```

Add `~/.local/bin` to your `PATH`.

For auto-complete of `jboi` from your shell add `eval "$(jboi --bash-completion-script jboi)"` to your `.bashrc`.

For shell auto-completion from ZSH add:
```
autoload -U +X compinit && compinit
autoload -U +X bashcompinit && bashcompinit
eval "$(jboi --bash-completion-script jboi)"
```
to your `.zshrc`.


## Licensing

All written work in docs (most notably
the [semantic standard](docs/lojban-spec.org)) is licensed under
the
[Creative Commons Attribution 4.0 International license](https://creativecommons.org/licenses/by/4.0/).
Modifications and derivative works are highly encouraged.  If you make
anything interesting with/from it or have any interesting ideas
for/about it, I'd love to hear about them.

The code is licensed under the [BSD3](LICENSE/BSD3) license.  It may also be licensed under the [MIT](LICENSE/MIT) license or [APACHE2](LICENSE/APACHE) license (NOTE: this repository contains code that may have different licenses. While they're likely compatible, ensure that any activities related to code from `src/datalog`, `src/dictionaries`, and `src/lojbanParser-0.1.9.2` comply with their licenses)
