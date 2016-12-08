# lo tegadri

[![Build Status](https://travis-ci.com/MarkMcCaskey/lo-tegadri.svg?token=JRN5sRswefsvVxT2unVR&branch=master)](https://travis-ci.com/MarkMcCaskey/lo-tegadri)

WARNING: this project is in early development and is likely unsafe and broken.  Expect bugs.  Contributions welcome.

All current design and implementation details are subject to change.

## About

An implementation of a semantic Lojban standard (currently unnamed).

This project is unofficial and is not affiliated with the LLG.

[[Lojban](https://mw.lojban.org/papri/Lojban)] is a constructed language with many properties that are likely valuable for human-computer interaction.

### Motivation

The original design goal was to create a generic, fleixbile, and speakable interface to structured-data traversal in a manner similar to the [[lens](https://hackage.haskell.org/package/lens)] library.  As the project progressed, it's changed significantly and is now closer to a logic programming language.  (There are definitely similarities between the type of traversal done by Datalog and the types of traversals done by lens-like objects.  This is a topic worth exploring.)  As such, the project is currently in a state of active research and does not currently have a clear path to completion.  As the system becomes useful, such a path may become clear.

## Installation

You will need [[stack](https://docs.haskellstack.org/en/stable/README/)] to build this project.


To install the project:
```
git clone https://github.com/MarkMcCaskey/lo-tegadri
cd lo-tegadri
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

