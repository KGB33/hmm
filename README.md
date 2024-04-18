# `Hmm` (Haskell Mux Manager)

`hmm` starts/attaches a tmux session in the current working directory named
after the git context. Useful when working with git worktrees.

By default, the session will be named `<REPO>-<BRANCH>`, but either can be
overriden using the relivant flags. Importantly, the repo name is generated
from the directory name on disk, not the remote name.


```
$ hmm --help
Usage: hmm [-r|--repo REPO] [-b|--branch BRANCH]

Available options:
  -r,--repo REPO           First part of the tmux session name.
  -b,--branch BRANCH       Second part of the tmux session name.
  -h,--help                Show this help text
```
