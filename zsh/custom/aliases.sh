alias open="xdg-open"
alias q="exit"
alias r="ranger-cd"
alias lsmnt="findmnt"

alias cdiff="colordiff"

alias l='ls -a'
alias ll='ls -la'

alias ec="emacsclient -n -c"

#mvlink .inputrc Dropbox/
function mvlink {
    mv "$1" "$2"
    ln -s "$2" "$1"
}

# alias copycmakefile="cp /home/erik/dotfiles/makefiles/c-makefile makefile"
function copymake {
    cp -L "/home/erik/dotfiles/makefiles/$1-makefile" ./makefile
}

function mounterik {
    sudo mount -t cifs "//192.168.1.$1/$2" "$3" -o user='erik rimskog'
}

function mkdircd {
    mkdir "$1" && cd "$1"
}
