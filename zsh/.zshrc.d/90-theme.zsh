# Modified version of https://github.com/ohmyzsh/ohmyzsh/blob/master/themes/bira.zsh-theme
# ZSH Theme - Preview: http://gyazo.com/8becc8a7ed5ab54a0262a470555c3eed.png

if [[ $UID -eq 0 ]]; then
    user_host='%B%F{red}%n@%m%f%b'
    user_symbol='%B#%b'
else
    user_host='%B%F{green}%n@%m%f%b'
    user_symbol='%B$%b'
fi

if [[ $SSH_CLIENT || $SSH_CONNECTION || $SSH_TTY ]]; then
    user_host='%B%F{yellow}%n@%m%f%b'
fi

# This is a highly condensed version of the plugin provided by oh-my-zsh
# https://github.com/ohmyzsh/ohmyzsh/blob/master/plugins/shrink-path/README.md
# I created my own because the oh-my-zsh version always expands a glob on every directory
# in the PWD, effectively running ls on them every time the prompt is displayed, which I
# don't want. This version is dumber and shortens the path simply by string manipulations.
function shrink_path {
    typeset -i lastfull=2
    typeset -i complen=2

    typeset -a components
    typeset result dir=${1-$PWD}

    dir=${dir/#$HOME/\~}
    components=(${(s:/:)dir})

    if [[ $components[1] == \~* ]]; then
       result=$components[1]
       shift components
    fi

    for comp in $components; do
        if (( $#components <= lastfull )); then
            result+=/$comp
        elif [[ $comp == .* ]]; then
            result+=/$comp[1,(( complen+1 ))]
        else
            result+=/$comp[1,$complen]
        fi
        shift components
    done
    echo ${result:-/}
}

current_dir='%B%F{blue}$(shrink_path)%f%b'

function exit_code_prompt {
    echo -n "%(?..──[%B%F{red}%?%f%b])"
}

function ranger_prompt {
    if [[ $RANGER_LEVEL ]]; then
        echo -n "──[%F{cyan}RANGER%f]"
    fi
}

function wine_prompt {
    if [[ $WINEPREFIX ]]; then
        echo -n "──[%F{red}$WINEPREFIX%f]"
    fi
}

function virtualenv_prompt {
    if [[ $VIRTUAL_ENV ]]; then
        echo -n "──[%F{magenta}${VIRTUAL_ENV:t}%f]"
    fi
}

PROMPT="┌─[${user_host}]──[${current_dir}]"'$(exit_code_prompt)$(ranger_prompt)$(wine_prompt)$(virtualenv_prompt)'"
└─${user_symbol} "

unset user_host
unset user_symbol
unset current_dir
