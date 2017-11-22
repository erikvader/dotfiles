alias ec="emacsclient -n"
alias copycmakefile="cp /home/erik/Dropbox/program\ settings/makefiles/c-makefile makefile"
alias gotoschool="cd /home/erik/Dropbox/skol-grejer/Universitet"
alias nemo="nemo --no-desktop"
alias e="exit"
alias r="ranger"

#mvlink .inputrc Dropbox/
function mvlink {
    mv "$1" "$2"
    ln -s "$2" "$1"
}

