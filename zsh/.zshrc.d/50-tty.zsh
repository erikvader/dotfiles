# Turn off the C-s and C-q bindings that pause and unpause the terminal
stty -ixon || true >/dev/null 2>&1
