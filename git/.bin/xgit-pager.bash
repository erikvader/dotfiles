# This script mimics what git does when it needs a pager, as described in the config
# core.pager. Here is the actual source code
# https://github.com/git/git/blob/ab689ea7f91ab0858e85776f31102203d3ea7b83/pager.c#L85
# This is primarily intended to make custom scripts behave like git, regarding the pager.
# This file should be sourced at the very top since it will potentially re-run the current
# script.

source xgit-assume-tty.bash

function _xgit-run-in-pager {
    local -
    set -euo pipefail

    # NOTE: this is not in standard git.
    if [[ ${XGIT_ASSUME_TTY-} == yes ]]; then
        xgit-set-assume-tty
        return
    fi

    if [[ ! -t 1 ]]; then
        return
    fi

    if [[ ! -v LESS ]]; then
        export LESS=FRX # --quit-if-one-screen --RAW-CONTROL-CHARS --no-init
    fi

    local pager
    if [[ -v GIT_PAGER ]]; then
        pager=$GIT_PAGER
    elif pager=$(git config core.pager); then
        :
    elif [[ -v PAGER ]]; then
        pager=$PAGER
    else
        pager=less
    fi

    if [[ $pager != cat && -n $pager ]]; then
        # NOTE: this is not in standard git. Stdout has already been checked above. This
        # is primarily to force colors into the pager. See xgit-color.bash
        export XGIT_ASSUME_TTY=yes
        "$0" "$@" | eval "$pager"
        exit $?
    fi
}

_xgit-run-in-pager "$@"
unset -f _xgit-run-in-pager
