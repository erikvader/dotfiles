alias open="xdg-open"
alias q="exit"
alias r="ranger-cd"
alias p="thunar &!"
alias lsmnt="findmnt -t ext4,cifs,vfat,ntfs,fuseblk,fuse -l"

alias tr='transmission-remote'

alias cdm='cd /media'
alias findbroken='find . -xtype l'

alias pps='ps -Ho pid,ppid,pgid,comm'
alias ppss='ps -Ho pid,ppid,pgid,command'

alias lsnet="sudo nmap -sn 192.168.1.0/24"

alias cdiff="diff --color=auto"

alias xa="xarchiver"

alias mi="mediainfo"

alias rsd='rsync -avhn --delete'
alias rs='rsync -avh --delete --progress'

alias y='yaourt'
alias yy='yaourt -Syy'
alias yu='yaourt -Syua'
alias yua='yaourt -Syua --noconfirm'
alias ys='yaourt -S'
alias yqs='yaourt -Qs'
alias yss='yaourt -Ss'
alias ysi='yaourt -Si'
alias yqi='yaourt -Qi'
alias yfs='yaourt -Fs'
alias yfl='yaourt -Fl'
alias ym='yaourt -Qm'
alias yo='yaourt -Qdt'
alias yl='yaourt -Qet'
alias yc='yaourt -Sc'
alias yr='yaourt -Rsn'
alias yR='yaourt -Rsnc'
alias ylibc="yaourt -S --aur --m-arg '--nocheck --skippgpcheck' libc++"

alias l='ls -Av'
alias ll='els -s'
alias lle='els -se'
alias lll='ls -lAhv'

alias s='lll | grep'

alias ec="emacsclient -n -c"

alias mountanime="mountsmb ERIKRIMSKOG anime /media/anime"

function mountsmb {
    if [[ $# -ne 3 ]]; then
        echo "Usage: $0 share-location share-name mountpoint" >&2
        return 1
    fi
    sudo mount -t cifs "//$1/$2" "$3" -o user="erik rimskog",file_mode=0644,dir_mode=0755,uid="$(id -u)",gid="$(id -g)" && cd "$3"
}

function mountfat {
    if [[ $# -ne 2 ]]; then
        echo "Usage: $0 device mountpoint" >&2
        return 1
    fi
    sudo mount "$1" "$2" -o uid="$(id -u)",gid="$(id -g)",umask=133,dmask=022 && cd "$2"
}

function ediff {
    emacsclient -n -c -e '(same-buffer (diff "'"$1"'" "'"$2"'"))' >/dev/null
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

function cless {
    pygmentize "$1" | less
}

C_LOCATIONS="$HOME/.clocations"

function ca {
    echo "${PWD/#$HOME/~}" >> "$C_LOCATIONS"
    sort -u -o "$C_LOCATIONS" "$C_LOCATIONS"
}

function c {
    x=$(cat "$C_LOCATIONS" | fzf +m --reverse --query="$1")
    if [[ -n "$x" ]]; then
        cd "${x/#\~/$HOME}"
    fi
}
