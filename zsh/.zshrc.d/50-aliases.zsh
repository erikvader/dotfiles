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

alias cclip='xclip -selection clipboard'

alias du='du -h'
alias free='free -h'

alias ip='ip --color=auto'

alias rsd='rsync -avh --dry-run --delete'
alias rs='rsync -avh --delete --progress'
alias rc='rsync -avh --progress'

alias pactree='pactree --color'
alias pc='paccache -rv' # Keep three latest package versions in cache
alias pcu='pc -uk0' # Remove everything of uninstalled packages
alias yc='y -Sc' # More aggressive package clean, but also cleans AUR and repos
alias y='yay'
alias ys='y -Sy'
alias ysd='ys --asdeps' # If installing an optional dependency of something
alias yr='y -Rn'
alias yrd='yr -s' # Remove, including dependencies
alias yru='yr -c' # Remove, including usages
alias yrdu='yr -sc'
alias yrud=yrdu
alias yo='y -Qdt' # List orphans
alias yor='yo -q | yr -'
alias ye='y -Qet' # List explicitly installed top-level packages, there can be hidden orphans here
alias ykr='ys archlinux-keyring'

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
