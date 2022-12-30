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
