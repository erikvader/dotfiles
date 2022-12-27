# Auto startx depending on the tty
# https://wiki.archlinux.org/title/xinit#Autostart_X_at_login
if [[ -z "${DISPLAY}" && "${XDG_VTNR}" -eq 1 ]]; then
    startx
fi
