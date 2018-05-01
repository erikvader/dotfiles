alias open="xdg-open"
alias q="exit"
alias r="ranger-cd"
alias p="pcmanfm &!"
alias lsmnt="findmnt"

alias cdiff="colordiff"

alias ao="xarchiver"

# change output dir with -C
# --keep-old-files, --keep-newer-files
alias amt="tar -cvf"
alias aut="tar -xvf"
alias amtg="tar -zcvf"
alias autg="tar -zxvf"

# -c to write to stdout and pipe to other file
alias amg="gzip"
alias aug="gunzip"

alias l='ls -A'
alias ll='ls -lA'

alias ec="emacsclient -n -c"

# alias copycmakefile="cp /home/erik/dotfiles/makefiles/c-makefile makefile"
function copymake {
    cp -L "/home/erik/dotfiles/makefiles/$1-makefile" ./makefile
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
}

function mkdircd {
    mkdir "$1" && cd "$1"
}
