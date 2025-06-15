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
alias pkill='pkill -e'

alias cclip="xclip -selection clipboard"

alias ip="ip --color=auto"

alias rsd='rsync -avh --dry-run --delete'
alias rs='rsync -avh --delete --progress'
alias rc='rsync -avh --progress'

alias y='yay'
alias ys='yay -S'
alias yc='yay -Sc'
alias yr='yay -Rn'
alias yrd='yr -s'
alias yru='yr -c'
alias yrdu='yr -sc'
alias yrud=yrdu
alias yo='yay -Qdt'
alias yor='yo -q | yr -'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias l='ls -avh --quoting-style=literal'
alias ll='els'
alias lll="l -l --time-style=$'+%d/%m  %Y\n%d/%m %H:%M'"

alias naspoweroff='umount /media/NAS && ssh -t nas "sudo poweroff"'
alias tvpoweroff='ssh -t tv@TV "sudo poweroff"'
alias tvsuspend='ssh -t tv@TV "sudo systemctl suspend"'
alias tvwakeup='wol 3c:97:0e:b1:53:44'

# TODO: gör en funktion som tar emot kryptiska argument som alias
alias SS='sudo systemctl'
alias SU='systemctl --user'
