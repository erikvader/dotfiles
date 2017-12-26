alias open="xdg-open"
alias gotoschool="cd /home/erik/Dropbox/skol-grejer/Universitet"
alias nemo="nemo --no-desktop"
alias e="exit"
alias r="ranger"
alias l='ls++ --psf -a'
alias la='ls++ --potsf -a'

alias ecb="emacsclient -n"
function ec {
    i3-msg -q workspace "2"
    emacsclient -n "$1"
}

#mvlink .inputrc Dropbox/
function mvlink {
    mv "$1" "$2"
    ln -s "$2" "$1"
}

# alias copycmakefile="cp /home/erik/Dropbox/program\ settings/makefiles/c-makefile makefile"
function copymake {
    cp -L "/home/erik/Dropbox/program settings/makefiles/$1-makefile" ./makefile
}
