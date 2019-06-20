#exec i3

function run {
    if ! pgrep $1 ;
    then
        $@&
    fi
}

while true
do
    VOL=$(amixer get Master | tail -1 | sed 's/.*\[\([0-9]*%\)\].*/\1/')
    LOCALTIME=$(date +%d-%m-%y\ %H:%M)
    TEMP="$(($(cat /sys/class/thermal/thermal_zone0/temp) / 1000))C"

        xsetroot -name " $TEMP | $LOCALTIME"

    sleep 5s
done &

eval $(gnome-keyring-daemon --start)
export SSH_AUTH_SOCK

while true; do
    run compton
    run nm-applet
    run blueman-applet
    run cbatticon -n
    run pasystray

    killall nextcloud
    run nextcloud

    run ~/.packages/arch_touchpad_setup/arch_touchpad_setup.bash
    feh --randomize --bg-fill ~/wallpapers/*

    dwm 2> ~/.dwm.log
#dwm >/dev/null 2>&1
done

