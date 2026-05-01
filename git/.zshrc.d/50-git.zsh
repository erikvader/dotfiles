# NOTE: this file should work in bash as well

# Show more things when autocompleting git
GIT_COMPLETION_SHOW_ALL=1
GIT_COMPLETION_SHOW_ALL_COMMANDS=1

alias g=git

gcd() {
    local p
    if p=$(xgit-submodule-cd "$@"); then
        cd "$p"
    else
        return 1
    fi

}
