# NOTE: can also use typeset -U path PATH and typeset -T PATH path :
[[ ":$PATH:" != *":$HOME/.bin:"* ]]       && export PATH="$HOME/.bin:$PATH"
[[ ":$PATH:" != *":$HOME/.local/bin:"* ]] && export PATH="$HOME/.local/bin:$PATH"
