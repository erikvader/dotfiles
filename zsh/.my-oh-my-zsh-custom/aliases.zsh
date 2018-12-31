alias open="xdg-open"
alias q="exit"
alias r="ranger-cd"
alias lsmnt="findmnt -t ext4,cifs,vfat,ntfs,fuseblk,fuse -l"

alias clone='setsid urxvt &>/dev/null'

alias ydm='youtube-dl --merge-output-format mkv --write-thumbnail'

alias pps='ps -Ho pid,ppid,pgid,comm'
alias ppss='ps -Ho pid,ppid,pgid,command'

alias lsnet="sudo nmap -sn 192.168.1.0/24"

alias cdiff="diff --color=auto"

alias mi="mediainfo"

alias cclip="xclip -selection clipboard"

alias rsd='rsync -avhsn --delete'
alias rs='rsync -avhs --delete --progress'
alias rc='rsync -avhs --progress'

alias y='yay'
alias ys='yay -S'
alias yo='yay -Qdt' # list orphans
alias yor='yay -Rn $(yay -Qqdt)' # remove orphans non-recursively
alias yr='yay -Rn'
alias yrr='yay -Rsn'
alias yrrr='yay -Rsnc'
alias ylibc="yay -S --aur --mflags '--nocheck' libc++ libc++abi libc++experimental"

alias l='ls -Av'
alias ll='els -s'
alias lle='els -se'
alias lll='ls -lAhv'

alias lg='lll | grep -i'

function f {
    find . -iname '*'"$1"'*'
}

# copy of glola
alias gl='git log --graph --pretty='"'"'%Cred%h%Creset -%C(auto)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset'"'"' --all'
alias gst='git status'
alias gsts='git submodule foreach "git status"'

alias SS='sudo systemctl'

alias ec="emacsclient -n -c"
alias sec="SUDO_EDITOR='emacsclient -c' sudo -e"

alias mountanime="mountsmb ERIKRIMSKOG anime erik /media/anime"

REPOS=( "$HOME/.emacs.d" "$HOME/dotfiles" "$HOME/.config/qutebrowser" )
function repos_cmd {
    for re in "${REPOS[@]}"; do
        if [[ ! -d "$re/.git" ]]; then
            echo "\"$re\" is not a git repo" >&2
            continue
        fi
        tput setaf 6
        echo "-----$(basename "$re")-----"
        tput sgr0
        (cd "$re" && "$@")
        echo
    done
}

alias repos_check='repos_cmd git fetch &>/dev/null; repos_cmd git status'
alias repos_pull='repos_cmd git pull'
alias repos_status='repos_cmd git status'
alias repos_magit='repos_cmd magit >/dev/null'

function mountsmb {
    if [[ $# -ne 4 ]]; then
        echo "Usage: $0 share-location share-name share-username mountpoint" >&2
        return 1
    fi
    sudo mount -t cifs "//$1/$2" "$4" -o user="$3",file_mode=0644,dir_mode=0755,uid="$(id -u)",gid="$(id -g)" && cd "$4"
}

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

function vdiff {
    emacsclient -n -c -e '(vdiff-files "'"$1"'" "'"$2"'" nil (function vdiff-close-everything))' >/dev/null
}

function magit {
    emacsclient -n -c -e '(same-buffer (magit-status))' >/dev/null
}

function copymake {
    local f="$HOME/makefiles/$1-makefile"
    if [[ ! -f "$f" ]]; then
        echo "\"$f\" doesn't exist" >&2
        return 1
    fi
    cp -L "$f" ./makefile
}

function mc {
    mkdir "$1" && cd "$1"
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

function men {
    if [[ $# -lt 1 ]]; then
        echo "usage $0 args" >&2
        return 1
    fi
    emacsclient -n -c -e '(same-buffer (man "'"$@"'"))' >/dev/null
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
