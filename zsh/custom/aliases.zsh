alias open="xdg-open"
alias q="exit"
alias r="ranger-cd"
alias p="thunar &!"
alias lsmnt="findmnt -t ext4,cifs,vfat,ntfs,fuseblk -l"

alias lsnet="sudo nmap -sn 192.168.1.0/24"

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

alias mountanime='sudo mount -t cifs "//ERIKRIMSKOG/anime" "/media/anime" -o user="erik rimskog",file_mode=0644,dir_mode=0755,uid="$(id -u)",gid="$(id -g)"'

function mountfat {
    sudo mount "$1" "$2" -o uid=$(id -u),gid=$(id -g),umask=133,dmask=022
}

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

function mc {
    mkdir "$1" && cd "$1"
}
