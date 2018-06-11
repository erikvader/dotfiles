alias open="xdg-open"
alias q="exit"
alias r="ranger-cd"
alias p="thunar &!"
alias lsmnt="findmnt -t ext4,cifs -l"

alias cdiff="colordiff"

alias xa="xarchiver"

alias mi="mediainfo"

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
alias yo='yaourt -Qdt'
alias yl='yaourt -Qet'
alias yc='yaourt -Sc'
alias yr='yaourt -Rsn'
alias yR='yaourt -Rsnc'

alias l='ls -A'
alias ll='els'
alias lle='els --list-empty'
alias lll='ls -lAh'

alias ec="emacsclient -n -c"

function ediff {
    emacsclient -n -e '(diff "'"$1"'" "'"$2"'")' >/dev/null
}

function copymake {
    local f="$HOME/makefiles/$1-makefile"
    if [[ ! -f "$f" ]]; then
        echo "\"$f\" doesn't exist" >&2
        return 1
    fi
    cp -L "$f" ./makefile
}

function mounterik {
    if [[ $# -eq 0 ]]; then
        echo "give me arguments plz" 1>&2
        return 1
    fi
    ip="$(nmblookup ERIKRIMSKOG | grep '192.168.1' | cut -d' ' -f1)"
    if ! [[ -n "$ip" && "${pipestatus[1]}" -eq 0 ]]; then
        echo "couldn't find an ip" 1>&2
        return 1
    fi
    sudo mkdir -p "/media/ERIKRIMSKOG/$1" &>/dev/null
    if ! [[ -d "/media/ERIKRIMSKOG/$1" ]]; then
        echo "failed to create dir" 1>&2
        return 1
    fi
    sudo mount -t cifs "//$ip/$1" "/media/ERIKRIMSKOG/$1" -o user='erik rimskog'
    cd "/media/ERIKRIMSKOG/$1"
}

function umounterik {
    if [[ $# -eq 0 ]]; then
        echo "give me arguments plz" 1>&2
        return 1
    fi
    sudo umount -lf "/media/ERIKRIMSKOG/$1"
    [[ -z "$(ls -A -1 /media/ERIKRIMSKOG/$1)" ]] && sudo rmdir "/media/ERIKRIMSKOG/$1"
    [[ -z "$(ls -A -1 /media/ERIKRIMSKOG)" ]] && sudo rmdir "/media/ERIKRIMSKOG"

    if echo "$PWD" | grep '/media/ERIKRIMSKOG' &>/dev/null; then
        cd '/media'
    fi
}

function mc {
    mkdir "$1" && cd "$1"
}
