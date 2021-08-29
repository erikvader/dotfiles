# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME=bira
setopt prompt_subst

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
DISABLE_AUTO_TITLE=true

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# fzf stuff
# $args $ignore_list $restrictions
export FZF_BASE_COMMAND='find -L . -mindepth 1 %s \\( -fstype dev -o -fstype proc %s \\) -prune -o \\( -name .git -o -name dosdevices \\) -prune -printf "%%P\\n" -o %s -printf "%%P\\n" 2>/dev/null'

export FZF_DEFAULT_COMMAND=$(printf "$FZF_BASE_COMMAND" "" "" "")
# export FZF_ALT_C_COMMAND=$(printf "$FZF_BASE_COMMAND" "" "" "-type d")
export FZF_CTRL_T_COMMAND=$(printf "$FZF_BASE_COMMAND" "-maxdepth 1 " "" "")

# Would you like to use another custom folder than $ZSH/custom?
ZSH_CUSTOM=$HOME/.my-oh-my-zsh-custom

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git shrink-path fzf docker)

source $ZSH/oh-my-zsh.sh

# User configuration

setopt HIST_IGNORE_SPACE
# unsetopt extended_glob
unsetopt autocd

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi
#export EDITOR=vim

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
source $ZSH_CUSTOM/aliases.zsh

# source /usr/share/autojump/autojump.zsh

# to make C-S-t work in termite
# source /etc/profile.d/vte*.sh

##################################### vim #####################################
# https://github.com/robbyrussell/oh-my-zsh/blob/master/plugins/vi-mode/vi-mode.plugin.zsh

# Updates editor information when the keymap changes.
function zle-keymap-select() {
  zle reset-prompt
  zle -R
}

# Ensure that the prompt is redrawn when the terminal size changes.
TRAPWINCH() {
  zle &&  zle -R
}

zle -N zle-keymap-select
# bindkey -v
# zle -N edit-command-line

export KEYTIMEOUT=1

autoload -U select-word-style
select-word-style default
zle -N backward-kill-space-word backward-kill-word-match
zstyle :zle:backward-kill-space-word word-style space
bindkey '^[w' backward-kill-space-word

bindkey '^[B' vi-backward-blank-word
bindkey '^[F' vi-forward-blank-word

# # allow v to edit the command line (standard behaviour)
# autoload -Uz edit-command-line
# bindkey -M vicmd 'v' edit-command-line

# # allow ctrl-p, ctrl-n for navigate history (standard behaviour)
# bindkey '^P' up-history
# bindkey '^N' down-history

# # allow ctrl-h, ctrl-w, ctrl-? for char and word deletion (standard behaviour)
# bindkey '^?' backward-delete-char
# bindkey '^h' backward-delete-char
# bindkey '^w' backward-kill-word

# # allow ctrl-r to perform backward search in history
# bindkey '^r' history-incremental-search-backward

# allow ctrl-a and ctrl-e to move to beginning/end of line
# bindkey '^a' beginning-of-line
# bindkey '^e' end-of-line
# bindkey '^k' kill-line

# # if mode indicator wasn't setup by theme, define default
# if [[ "$MODE_INDICATOR" == "" ]]; then
#   MODE_INDICATOR="%{$fg_bold[red]%}<%{$fg[red]%}<<%{$reset_color%}"
# fi

# function vi_mode_prompt_info() {
#   echo "${${KEYMAP/vicmd/$MODE_INDICATOR}/(main|viins)/}"
# }

# # define right prompt, if it wasn't defined by a theme
# if [[ "$RPS1" == "" && "$RPROMPT" == "" ]]; then
#   RPS1='$(vi_mode_prompt_info)'
# fi

function ranger-cd {
    temp="$(mktemp)"

    SHELL=r.shell ranger --choosedir "$temp"

    content="$(cat "$temp" 2>/dev/null)" &&
        if [[ "$content" ]]; then
            cd "$content"
        fi

    rm -f "$temp"
}

bindkey -s '^o' '\eq ranger-cd\n'

# smartcase in less
export LESS=-Ri

# bat default style
export BAT_STYLE=changes

# ranger wont load default conf, only user
export RANGER_LOAD_DEFAULT_RC=FALSE

# disables prompt mangling in virtual_env/bin/activate
export VIRTUAL_ENV_DISABLE_PROMPT=1

# which cowfortune &>/dev/null && cowfortune

# use custom dircolors if there are any
if [[ -f "$HOME/.dircolors" ]]; then
    eval "$(dircolors -b "$HOME/.dircolors")"
    zstyle ':completion:*' list-colors "${(s.:.)LS_COLORS}"
fi

if [[ -f "$HOME/.zshrc.local" ]]; then
    source "$HOME/.zshrc.local"
fi

# run a command and enter normal shell after it exits
# instead of exiting the while shell
# http://www.zsh.org/mla/users/2005/msg00599.html
# zsh -is eval cmd...
if [[ $1 == eval ]]; then
    "$@"
    set --
fi

# Emacs tramp fix
if [[ $TERM = dumb ]]; then
    unsetopt zle
    unsetopt prompt_cr
    unsetopt prompt_subst
    PS1='$ '
fi
