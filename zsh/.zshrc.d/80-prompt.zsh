if [[ $intelligence == emacs ]]; then
    # Tell emacs about the current directory instead of letting it track it itself
    printf_osc7() {
        printf "\e]7;file://%s%s\e\\" "$HOST" "$PWD"
    }
    autoload -Uz add-zsh-hook
    add-zsh-hook precmd printf_osc7
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

# enable variable substitutions and stuff in the prompt
setopt prompt_subst

# dumb simple default prompt
PROMPT='$ '
