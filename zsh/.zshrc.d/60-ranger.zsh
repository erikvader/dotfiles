function ranger-cd {
    temp="$(mktemp)"

    SHELL=r.shell ranger --choosedir "$temp"

    content="$(cat "$temp" 2>/dev/null)" &&
        if [[ "$content" ]]; then
            cd "$content"
        fi

    rm -f "$temp"
}

bindkey -s '^o' '\eq ranger-cd\n'

# ranger wont load default conf, only user
export RANGER_LOAD_DEFAULT_RC=FALSE
