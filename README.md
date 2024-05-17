# `Hmm` (Haskell Mux Manager)

`hmm` starts/attaches a tmux session in the current working directory named
after the git context. Useful when working with git worktrees.

By default, the session will be named `<REPO>-<BRANCH>`, but either can be
overridden using the relevant flags. Importantly, the repo name is generated
from the directory name on disk, not the remote name.


```
$ hmm --help
Usage: hmm [-r|--repo REPO] [-b|--branch BRANCH]

Available options:
  -r,--repo REPO           First part of the tmux session name.
  -b,--branch BRANCH       Second part of the tmux session name.
  -h,--help                Show this help text
```

# Install

The preferred installation method is via Nix flakes.

To quickly try it out:

```shell
nix shell 'github:kgb33/hmm#hmm'
```

Or add it to a `flake.nix`:

```nix
inputs.hmm = {
  url = "github:KGB33/hmm";
  inputs.nixpkgs.follows = "nixpkgs";
};
```

To build/run from source, `git clone` then:

```
cabal build
cabal run
```
