autoload -Uz compinit
compinit

# enable the cool menu
zstyle ':completion:*' menu select

# do not autoselect the first entry
unsetopt menu_complete

# this also enable the menu?
setopt auto_menu

# how to handle completion in the middle of a word
setopt complete_in_word
setopt always_to_end

# complete case insensitively
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'

# complete . and ..
zstyle ':completion:*' special-dirs true

# load bash completions
autoload -U +X bashcompinit && bashcompinit

# set colors for completion
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# expand aliases with tab
zstyle ':completion:*' completer _expand_alias _complete _ignored
