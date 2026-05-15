if [[ $intelligence != smart ]]; then
    unsetopt zle
    return
fi

# Ensure that the prompt is redrawn when the terminal size changes.
TRAPWINCH() {
    zle && zle -R
}

# Needed to make the terminal change mode to recognize the codes from terminfo
# https://invisible-island.net/xterm/xterm.faq.html#xterm_arrows
if (( ${+terminfo[smkx]} )) && (( ${+terminfo[rmkx]} )); then
    function zle-line-init() {
        echoti smkx
    }
    function zle-line-finish() {
        echoti rmkx
    }
    zle -N zle-line-init
    zle -N zle-line-finish
fi

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

# Switch to a history search that preserves the cursor position
bindkey '^[p' history-beginning-search-backward
bindkey '^[n' history-beginning-search-forward

# expand alias similarly to bash
bindkey '^[^e' _expand_alias

# make the delete key work as expected
bindkey "${terminfo[kdch1]}" delete-char

# edit the current cmdline in $EDITOR
autoload -U edit-command-line
zle -N edit-command-line
bindkey '\C-x\C-e' edit-command-line

