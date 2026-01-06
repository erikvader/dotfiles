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

alias pc='paccache -rv'
alias pcu='pc -uk0'
alias y='yay'
alias ys='y -Sy'
alias ysd='ys --asdeps'
alias yc='y -Sc'
alias yr='y -Rn'
alias yrd='yr -s'
alias yru='yr -c'
alias yrdu='yr -sc'
alias yrud=yrdu
alias yo='y -Qdt'
alias yor='yo -q | yr -'
alias ye='y -Qet'

alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias l='ls -Avh --quoting-style=shell-escape'
alias ll='l -la --time-style=long-iso'

alias naspoweroff='umount /media/NAS && ssh -t nas.local "sudo midclt call -j system.shutdown reason"'
alias naswakeup='echo TODO'
alias tvpoweroff='ssh -t tv@TV.local "sudo poweroff"'
alias tvwakeup='wol 3c:97:0e:b1:53:44'

alias SS='sudo systemctl'
alias SU='systemctl --user'
alias JU='journalctl --user --pager-end'
