# function search path
fpath=($HOME/.zshfpath.d $fpath)

# Starting process in the background
start() {
    setsid --fork "$@" >/dev/null 2>&1 </dev/null
}
