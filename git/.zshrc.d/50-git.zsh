# Show more things when autocompleting git
GIT_COMPLETION_SHOW_ALL=1
GIT_COMPLETION_SHOW_ALL_COMMANDS=1

# TODO: this could be a custom command that accepts a fuzzy search for the submodule to
# operate on. Perhaps integrate that in git-foreach instead?
alias g=git
# TODO: use DELTA_FEATURES instead?
alias gg='g -c delta.side-by-side=true'
alias gup='cd "$(git rev-parse --show-cdup)"'
alias gsuper='cd "$(git rev-parse --show-superproject-working-tree)"'

# TODO: en cd som bara autocompletar submoduler
