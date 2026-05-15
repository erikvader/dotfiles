if [[ $intelligence == dumb ]]; then
    return
fi

() {
    setopt local_options nounset

    local compact=yes
    local left=' '
    local right=
    if [[ $intelligence == smart ]]; then
        compact=no
        left='──['
        right=']'
    fi

    # Modified version of https://github.com/ohmyzsh/ohmyzsh/blob/master/themes/bira.zsh-theme
    # ZSH Theme - Preview: http://gyazo.com/8becc8a7ed5ab54a0262a470555c3eed.png

    local user_host=
    local user_symbol=
    if [[ $UID -eq 0 ]]; then
        user_host='%B%F{red}%n@%m%f%b'
        user_symbol='%B#%b'
    else
        user_host='%B%F{green}%n@%m%f%b'
        user_symbol='%B$%b'
    fi

    if [[ -v SSH_CLIENT || -v SSH_CONNECTION || -v SSH_TTY ]]; then
        user_host='%B%F{yellow}%n@%m%f%b'
    fi

    local current_dir="${left}%B%F{blue}\$(shrink_path)%f%b${right}"
    local exit_code="%(?..${left}%B%F{red}%?%f%b${right})"
    local ranger_prompt="\${RANGER_LEVEL:+${left}%F{cyan\}RANGER%f${right}}"
    local wine_prompt="\${WINEPREFIX:+${left}%F{red\}\${WINEPREFIX}%f${right}}"
    local virtualenv_prompt="\${VIRTUAL_ENV:+${left}%F{magenta\}\${VIRTUAL_ENV:t}%f${right}}"

    if [[ $compact == no ]]; then
        PROMPT="┌─[${user_host}]${current_dir}${ranger_prompt}${wine_prompt}${virtualenv_prompt}${exit_code}"$'\n'"└─${user_symbol} "
    else
        PROMPT="[${user_host}${current_dir}${ranger_prompt}${wine_prompt}${virtualenv_prompt}${exit_code}]${user_symbol} "
    fi
}
