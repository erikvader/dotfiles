# Modified version of https://github.com/ohmyzsh/ohmyzsh/blob/master/themes/bira.zsh-theme
# ZSH Theme - Preview: http://gyazo.com/8becc8a7ed5ab54a0262a470555c3eed.png

return_code="%(?..%{$fg[red]%}%? ↵%{$reset_color%})"

if [[ $UID -eq 0 ]]; then
    user_host='%{$terminfo[bold]$fg[red]%}%n@%m%{$reset_color%}'
    user_symbol='#'
else
    user_host='%{$terminfo[bold]$fg[green]%}%n@%m%{$reset_color%}'
    user_symbol='$'
fi

if [[ $SSH_CLIENT || $SSH_CONNECTION || $SSH_TTY ]]; then
    user_host='%{$terminfo[bold]$fg[yellow]%}%n@%m%{$reset_color%}'
fi

# https://stackoverflow.com/a/73590795
prompt_pwd () {
    local p=${${PWD:/~/\~}/#~\//\~/}
    echo -n "${(@j[/]M)${(@s[/]M)p##*/}##(.|)?}$p:t"
}

current_dir='%{$terminfo[bold]$fg[blue]%}$(prompt_pwd)%{$reset_color%}'

function ranger_prompt {
    if [[ $RANGER_LEVEL ]]; then
        echo -n "──[%{$fg[cyan]%}RANGER%{$reset_color%}]"
    fi
}

function wine_prompt {
    if [[ $WINEPREFIX ]]; then
        echo -n "──[%{$fg[red]%}$WINEPREFIX%{$reset_color%}]"
    fi
}

function virtualenv_prompt {
    if [[ $VIRTUAL_ENV ]]; then
        echo -n "──[%{$fg[magenta]%}${VIRTUAL_ENV:t}%{$reset_color%}]"
    fi
}

PROMPT="┌─[${user_host}]──[${current_dir}]"'$(ranger_prompt)$(wine_prompt)$(virtualenv_prompt)'"
└─%B${user_symbol}%b "
RPS1="%B${return_code}%b"

unset user_host
unset user_symbol
unset current_dir
