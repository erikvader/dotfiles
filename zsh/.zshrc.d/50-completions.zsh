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

# set colors for completion
zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"

# don't make these characters remove the auto completed space when inserted
ZLE_SPACE_SUFFIX_CHARS=$'&|'
