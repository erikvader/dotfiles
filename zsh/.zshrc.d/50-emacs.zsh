function ec {
    # If the argument is - then write stdin to a tempfile and open the
    # tempfile.
    if [[ $# -eq 1 && "$1" = - ]]; then
        tempfile="$(mktemp "emacs-stdin.XXXXXXX" --tmpdir)"
        cat - > "$tempfile"
        emacsclient -nc --eval "(progn (find-file \"$tempfile\") (set-visited-file-name nil) (rename-buffer \"*stdin*\" t) (delete-file \"$tempfile\"))" >/dev/null
    else
        emacsclient -nc "$@"
    fi
}

function ediff {
    emacsclient -n -c -e '(same-buffer (ediff "'"$1"'" "'"$2"'"))' >/dev/null
}

function emacsdiff {
    emacsclient -n -c -e '(same-buffer (diff "'"$1"'" "'"$2"'"))' >/dev/null
}

function magit {
    emacsclient -n -c -e '(same-buffer (magit-status))' >/dev/null
}

alias sec="SUDO_EDITOR='emacsclient -c' sudo -e"
alias dired='emacsclient -n -c -e "(dired \".\")" > /dev/null'
