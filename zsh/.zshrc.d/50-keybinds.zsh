# use emacs-style keybindings
bindkey -e

# TODO: already have?
#bindkey ' ' magic-space

# escape stuff when pasting
autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic

# TODO: ??
autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

# M-w kills back a WORD
autoload -U select-word-style
select-word-style default
zle -N backward-kill-space-word backward-kill-word-match
zstyle :zle:backward-kill-space-word word-style space
bindkey '^[w' backward-kill-space-word

# Move by WORDs
bindkey '^[B' vi-backward-blank-word
bindkey '^[F' vi-forward-blank-word

# TODO: ??
# export KEYTIMEOUT=1
