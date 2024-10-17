# disables prompt mangling in virtual_env/bin/activate
export VIRTUAL_ENV_DISABLE_PROMPT=1

# move all __pycache__
export PYTHONPYCACHEPREFIX=$HOME/.cache/pycache

# Link `pythonpath` and `PYTHONPATH` together, like `path`, and make sure only unique
# items are added
typeset -UT PYTHONPATH pythonpath :
pythonpath=($HOME/.pythonlibs $pythonpath)
export PYTHONPATH
