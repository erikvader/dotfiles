alias sd='sudo docker'
alias sdc='sudo docker-compose'

alias pps='ps -Ho pid,ppid,pgid,comm'
alias ppss='ps -Ho pid,ppid,pgid,command'
alias psc='ps -Ao pid,%cpu,%mem,comm --sort -%cpu | head -n 20'
alias psm='ps -Ao pid,%cpu,%mem,comm --sort -%mem | head -n 20'

alias diff="diff --color=auto"

alias pg='pgrep -l'

alias cclip="xclip -selection clipboard"

alias rsd='rsync -avhs --dry-run --delete'
alias rs='rsync -avhs --delete --progress'
alias rc='rsync -avhs --progress'

alias y='yay'
alias ys='yay -S'
alias yc='yay -Sc'
alias yr='yay -Rn'
alias yrr='yay -Rsn'
alias yrrr='yay -Rsnc'
alias ylibc="yay -S --aur --mflags '--nocheck' libc++ libc++abi libc++experimental"
alias yo='yay -Qdt' # list orphans

alias ..='cd ..'
alias l='ls -Av'
alias ll='els -s'
alias lle='els -se'
alias lll='ls -lAhv'

alias naspoweroff='umount /media/NAS && ssh -t nas "sudo poweroff"'

# copy of glola
alias gl='git log --graph --pretty='"'"'%C(yellow)%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'"'"
alias gla='gl --all'
alias gst='git status'
alias gsts='git submodule foreach "git status"'

alias SS='sudo systemctl'

REPOS=( "$HOME/.emacs.d" "$HOME/dotfiles" "$HOME/.config/qutebrowser" "$HOME/Documents/statusbar" "$HOME/.pythonlibs/emdb" )
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

function mountfat {
    if [[ $# -ne 2 ]]; then
        echo "Usage: $0 device mountpoint" >&2
        return 1
    fi
    sudo mount "$1" "$2" -o uid="$(id -u)",gid="$(id -g)",umask=033,dmask=022 && cd "$2"
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

function man {
    LESS_TERMCAP_md=$'\e[01;35m' \
    LESS_TERMCAP_me=$'\e[0m' \
    LESS_TERMCAP_se=$'\e[0m' \
    LESS_TERMCAP_so=$'\e[45;30m' \
    LESS_TERMCAP_ue=$'\e[0m' \
    LESS_TERMCAP_us=$'\e[01;34m' \
    command man "$@"
}

function copyfile {
    if [[ $# -ne 1 ]]; then
        echo "Usage: $0 file" >&2
        return 1
    fi
    if [[ ! -f $1 ]]; then
        echo "\"$1\" is not a regular file" >&2
        return 1
    fi
    local type=$(file -b --mime-type "$1")
    xclip -selection clipboard -t "$type" -i "$1"
}

alias sec="SUDO_EDITOR='emacsclient -c' sudo -e"

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

alias dired='emacsclient -n -c -e "(dired \".\")" > /dev/null'
