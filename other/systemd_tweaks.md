# /etc/systemd/journald.conf
## limit log size
```
SystemMaxUse=500M
```

# /etc/systemd/logind.conf
## kill all processes in session when loggin out
```
KillUserProcesses=yes
```
## behaviour on lid close
```
HandleLidSwitch=suspend
HandleLidSwitchDocked=ignore
```
docked is used when an external display is connected (maybe from other things as well?)
## don't do anything on power key
```
HandlePowerKey=ignore
```
