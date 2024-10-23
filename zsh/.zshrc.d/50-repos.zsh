REPOS=(
    $HOME/.emacs.d
    $HOME/dotfiles
    $HOME/.config/qutebrowser
    $HOME/Documents/statusbar
    $HOME/.pythonlibs/emdb
)

function repos_cmd {
    for re in "${REPOS[@]}"; do
        tput setaf 6
        echo "-----$(basename "$re")-----"
        tput sgr0
        if [[ ! -d "$re/.git" ]]; then
            echo "doesn't exist..." >&2
        else
            (cd "$re" && "$@")
        fi
        echo
    done
}

alias repos_check='repos_cmd git fetch &>/dev/null; repos_cmd git status'
alias repos_pull='repos_cmd git pull --ff-only'
alias repos_status='repos_cmd git status'
alias repos_magit='repos_cmd magit >/dev/null'
