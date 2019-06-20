
#function run {
#if ! pgrep $1 ;
#then
#$@&
#fi
#}

while true
do
    VOL=$(amixer get Master | tail -1 | sed 's/.*\[\([0-9]*%\)\].*/\1/')
    LOCALTIME=$(date +%d-%m-%y\ %H:%M)
    TEMP="$(($(cat /sys/class/thermal/thermal_zone0/temp) / 1000))C"

    xsetroot -name " $TEMP | $LOCALTIME"

    sleep 5s
done &


while true; do
    killall compton; compton &
    killall nm-applet; nm-applet &
    killall blueman-applet; blueman-applet &
    killall cbatticon; cbatticon -n &
    killall pa-applet; pa-applet &

    ck-launch-session dbus-launch --sh-syntax --exit-with-x11 dwm
done

#while true; do
#run compton
#run nm-applet
#run blueman-applet
#run cbatticon -n
#run pasystray

   # killall nextcloud
   # run nextcloud

   # feh --randomize --bg-fill ~/wallpapers/*

    #dwm 2> ~/.dwm.log
    #dwm >/dev/null 2>&1
#done

