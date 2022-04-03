# usage:
# source snapshot_backup.bash
# add blah blah
# add blah blah

set -e

snapshots=snapshots
curdate=$(date '+%Y-%m-%d#%H:%M')
prevdate=$(find "./$snapshots" -maxdepth 1 -mindepth 1 -type d -printf '%P\n' | LC_ALL=C sort -r | head -n1)

if [[ "$curdate" = "$prevdate" ]]; then
    echo "oopsies: prevdate == curdate" >&2
    exit 1
fi

function add {
    if [[ $# -lt 2 ]]; then
        echo invalid usage of add >&2
        exit 1
    fi

    local src=$1
    local target=./$snapshots/$curdate/$2
    local linkdest=$PWD/$snapshots/$prevdate/$2
    # local target=./$snapshots/$prevdate/$2
    # local linkdest=
    local exclude=
    local mover=

    shift 2
    while [[ "$1" ]]; do
        case "$1" in
            --mover) mover=t ;;
            *) exclude="$exclude$1"$'\n' ;;
        esac
        shift
    done

    mkdir -p "$target"
    if [[ -n $mover ]]; then
        cp -al -t "$target" "$linkdest/$(basename "$src")"
        mover "$target/$(basename "$src")" "$src" doit
    fi
    echo -e "\033[34m${src}\033[0m"
    # NOTE: if $linkdest doesn't exist, then rsync just complains and continues anyway
    rsync -avhs --delete --info=progress2 --safe-links --link-dest="$linkdest" --exclude-from=- "$src" "$target" <<< "$exclude"
}

function end {
    date '+%s' > last_backup
    echo
    findmnt --noheadings --mountpoint "$PWD" -o USE%,AVAIL | awk '{ print $1,"used,",$2,"left"}'
    notify-send 'Backup completed!'
}

trap end EXIT
