# Needed to make the terminal change mode to recognize the codes from terminfo
# https://invisible-island.net/xterm/xterm.faq.html#xterm_arrows
function zle-line-init() {
    echoti smkx
}
function zle-line-finish() {
    echoti rmkx
}
zle -N zle-line-init
zle -N zle-line-finish

# use word instead of WORDS, basically
WORDCHARS=

# use emacs-style keybindings
bindkey -e

# auto expand history
bindkey ' ' magic-space

# escape stuff when pasting
autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic

# escape URLs when typed or pasted
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

# up and down arrows search in history
autoload -U up-line-or-beginning-search
zle -N up-line-or-beginning-search
bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search

autoload -U down-line-or-beginning-search
zle -N down-line-or-beginning-search
bindkey "${terminfo[kcud1]}" down-line-or-beginning-search

# expand alias similarly to bash
bindkey '^[^e' _expand_alias
