alias ec="emacsclient"
alias copycmakefile="cp /home/erik/Dropbox/program\ settings/makefiles/c-makefile makefile"
alias gotoschool="cd /home/erik/Dropbox/skol-grejer/Universitet/åk\ 2"
alias nemo="nemo --no-desktop"

#mvlink .inputrc Dropbox/
function mvlink {
    mv "$1" "$2"
    ln -s "$2" "$1"
}

