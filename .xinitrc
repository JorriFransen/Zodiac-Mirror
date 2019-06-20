
while true
do
    VOL=$(amixer get Master | tail -1 | sed 's/.*\[\([0-9]*%\)\].*/\1/')
    LOCALTIME=$(date +%d-%m-%y\ %H:%M)
    TEMP="$(($(cat /sys/class/thermal/thermal_zone0/temp) / 1000))C"

    xsetroot -name " $TEMP | $LOCALTIME"

    sleep 5s
done &

dbus-update-activation-environment --systemd DISPLAY
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK 


while true; do
    dbus-launch --sh-syntax  dwm
done

