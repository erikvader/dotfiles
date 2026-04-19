function xgit-real-stdout {
    local -
    set -eu
    local -n _ref=$1
    local _dupfd
    exec {_dupfd}>&1
    _ref=$(stat --dereference --format='%d:%i' /proc/self/fd/$_dupfd)
    exec {_dupfd}>&-
}

function xgit-set-assume-tty {
    export XGIT_ASSUME_TTY
    xgit-real-stdout XGIT_ASSUME_TTY
}

function xgit-stdout-is-assumed-tty {
    local -
    set -eu
    if [[ -v XGIT_ASSUME_TTY ]]; then
        local stdout
        xgit-real-stdout stdout
        if [[ $XGIT_ASSUME_TTY == "$stdout" ]]; then
            return 0
        fi
    fi
    return 1
}
