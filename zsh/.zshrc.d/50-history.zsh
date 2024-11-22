HISTFILE=~/.zsh_history
HISTSIZE=50000
SAVEHIST=$HISTSIZE

# save timestamps of history
setopt extended_history

# don't save duplicates in history
setopt hist_ignore_all_dups

# don't save in history if the line starts with a space
setopt hist_ignore_space

# expand history entry before running it
setopt hist_verify

# save and load history to HISTFILE
setopt share_history
