# NOTE: can also use typeset -U path PATH and typeset -T PATH path :
[[ ":$PYTHONPATH:" != *":$HOME/.pythonlibs:"* ]] && export PYTHONPATH="$HOME/.pythonlibs${PYTHONPATH:+:}$PYTHONPATH"

# move all __pycache__
export PYTHONPYCACHEPREFIX=$HOME/.cache/pycache
