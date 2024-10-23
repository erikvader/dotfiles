alias sd='sudo docker'
alias sdc='sudo docker-compose'

alias pps='ps -Ho pid,ppid,pgid,comm'
alias ppss='ps -Ho pid,ppid,pgid,command'
alias psc='ps -Ao pid,%cpu,%mem,comm --sort -%cpu | head -n 20'
alias psm='ps -Ao pid,%cpu,%mem,comm --sort -%mem | head -n 20'

alias sss='ss -an'
alias sssp='sss -p'

alias pg='pgrep -li'
alias pga='pg -a'

alias cclip="xclip -selection clipboard"

alias rsd='rsync -avhs --dry-run --delete'
alias rs='rsync -avhs --delete --progress'
alias rc='rsync -avhs --progress'

alias y='yay'
alias ys='yay -S'
alias yc='yay -Sc'
alias yr='yay -Rn'
alias yrd='yr -s'
alias yru='yr -c'
alias yrdu='yr -sc'
alias yrud=yrdu
alias ylibc="yay -S --aur --mflags '--nocheck' libc++ libc++abi libc++experimental"
alias yo='yay -Qdt'
alias yor='yo -q | yr -'

alias ..='cd ..'
alias l='ls -Av'
alias ll='els -s'
alias lle='els -se'
alias lll='ls -lAhv'

alias naspoweroff='umount /media/NAS && ssh -t nas "sudo poweroff"'
alias tvpoweroff='ssh -t tv@TV "sudo poweroff"'
alias tvsuspend='ssh -t tv@TV "sudo systemctl suspend"'
alias tvwakeup='wol 3c:97:0e:b1:53:44'

alias g=git
alias gup='cd "$(git rev-parse --show-cdup)"'
alias gsuper='cd "$(git rev-parse --show-superproject-working-tree)"'

alias SS='sudo systemctl'
alias SU='systemctl --user'
