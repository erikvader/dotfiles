# A little bit like tput, but only outputs ANSI codes, and it inspects environment
# variables to force and unforce colors. Normal git doesn't refer to the terminfo
# database, so these scripts doesn't need to deal with that complexity.
# https://github.com/git/git/blob/master/color.c

# Defining GIT_ASSUME_TTY means that the caller assures that the stdout of this script
# will reach a tty eventually. Useful when the caller is redirecting the output using
# to a pager and it has already checked that its output is a tty.

# Note that the colors are only supposed to be printed to the current script's stdout.

source xgit-assume-tty.bash

xgit_color_enabled=false
function xgit-color { :; }

function _xgit-color-enabled {
    local -
    set -eu

    if [[ -n ${NO_COLOR-} ]]; then
        return 1
    fi

    if [[ -n ${FORCE_COLOR-} ]]; then
        return 0
    fi

    if [[ ${TERM:-dumb} == dumb ]]; then
        return 1
    fi

    if xgit-stdout-is-assumed-tty; then
        return 0
    fi

    if [[ -t 1 ]]; then
        return 0
    fi

    return 1
}

if _xgit-color-enabled; then
    xgit_color_enabled=true
    function xgit-color {
        local -
        set -eu

        if (($# <= 0)); then
            echo No arguments given
            return 1
        fi

        local -r csi=$'\e['
        local -r sgr=m
        local parts=

        while (($#>0)); do
            if [[ -n $parts ]]; then
                parts+=";"
            fi

            local -i code

            local category=$1
            shift
            case $category in
                fg-dark|fg) code=30 ;;
                bg-dark|bg) code=40 ;;
                fg-bright) code=90 ;;
                bg-bright) code=100 ;;
                bold)
                    parts+="1"
                    continue
                    ;;
                reset)
                    parts+="0"
                    continue
                    ;;
                *) echo "Invalid category '$category'" >&2; return 1 ;;
            esac

            local color=$1
            shift
            case $color in
                black) ((code+=0)) ;;
                red) ((code+=1)) ;;
                green) ((code+=2)) ;;
                yellow) ((code+=3)) ;;
                blue) ((code+=4)) ;;
                magenta) ((code+=5)) ;;
                cyan) ((code+=6)) ;;
                white) ((code+=7)) ;;
                *) echo "Invalid color '$color'" >&2; return 1 ;;
            esac

            parts+="$code"
        done

        echo -n "${csi}${parts}${sgr}"
    }
fi

unset _xgit-color-enabled
readonly xgit_color_enabled
