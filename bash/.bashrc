# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    # We have color support; assume it's compliant with Ecma-48
    # (ISO/IEC-6429). (Lack of such support is extremely rare, and such
    # a case would tend to support setf rather than setaf.)
    color_prompt=yes
    else
    color_prompt=
    fi
fi

# 4 ist för 3 för background
C_NC='\e[0m' # No Color
C_WHITE='\e[1;37m'
C_BLACK='\e[0;30m'
C_BLUE='\e[0;34m'
C_LIGHT_BLUE='\e[1;34m'
C_GREEN='\e[0;32m'
C_LIGHT_GREEN='\e[1;32m'
C_CYAN='\e[0;36m'
C_LIGHT_CYAN='\e[1;36m'
C_RED='\e[0;31m'
C_LIGHT_RED='\e[1;31m'
C_PURPLE='\e[0;35m'
C_LIGHT_PURPLE='\e[1;35m'
C_BROWN='\e[0;33m'
C_YELLOW='\e[1;33m'
C_GRAY='\e[0;30m'
C_LIGHT_GRAY='\e[0;37m'

if [ "$color_prompt" = yes ]; then
    #PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
    # If id command returns zero, youve root access.
    C_NAME=
    if [ $(id -u) -eq 0 ]; then # you are root, set red colour prompt
        C_NAME=$C_LIGHT_RED
    else # normal
        C_NAME=$C_LIGHT_GREEN
    fi

    PS1="${debian_chroot:+($debian_chroot)}$C_BLUE┏━━$C_LIGHT_BLUE($C_NAME\u@\H$C_LIGHT_BLUE)$C_BLUE━━━$C_LIGHT_BLUE($C_LIGHT_PURPLE\w$C_LIGHT_BLUE)$C_BLUE\n┗━$C_NC\$ "
    
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    #PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

#starta screenfetch
#screenfetch

export VISUAL=emacsclient
export EDITOR="$VISUAL"

#-------aliases--------
alias xopen='xdg-open'
alias perlgrep='grep -P'
alias rmtrash='gvfs-trash'
alias aptins='sudo apt install '
alias aptrem='sudo apt remove '
alias aptpur='sudo apt purge '
alias aptupd='sudo apt update'
alias aptupg='sudo apt upgrade'
alias aptdis='sudo apt dist-upgrade'
alias aptaut='sudo apt autoremove'
alias aptlup='sudo apt list --upgradable'
alias eclimd='~/bin/eclipse-neon/eclimd'
alias nemon='nemo --no-desktop'

#enter special monitor setup
#function fixmon {
#  xrandr --newmode "1280x1024_old"   108.00   1280 1328 1440 1688   1024 1025 1028 1066 +hsync +vsync
#  xrandr --addmode VGA1 "1280x1024_old"
#  #xrandr --output VGA1 --mode "1280x1024_old"
#
#}

#c-saker
function gcco {
   gcc -o $1 "$1.c"
}

function gccor {
    if gcco $1 ; then
        ./$1
    else
        echo ""
    fi
}

function gcca {
    python3 ~/auto_dependency_gcc.py $1
}

set_term_title(){
   echo -en "\033]0;$1\a"
}
