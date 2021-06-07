#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}


xrandr --output DP1 --off --output DP2 --off --output DP3 --mode 2880x1620 --pos 0x0 --rotate normal --output HDMI1 --off --output HDMI2 --off --output HDMI3 --off --output VIRTUAL1 --off

xsetroot -cursor_name left_ptr &
pkill -x sxhkd ; run sxhkd -c ~/.config/sxhkd/sxhkdrc &


## System applets and applications
#conky -c $HOME/.config/bspwm/system-overview &
run nm-applet &
numlockx on &
run blueman-applet &
picom -fb &
oneDouble & # only after picom should we invoke. 

# caps as esc or control when used with another key. 
setxkbmap -option ctrl:nocaps 
xcape -e 'Control_L=Escape'

xcape -e "Super_L=Super_L|space"

/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
run volumeicon &
#run pa-applet &
# nitrogen --restore &
feh --bg-scale ~/Pictures/wallpapers/OneDark/the-neon-shallows-oned.png &
run greenclip daemon &
run dunst &
$HOME/.config/polybar/launch.sh &

#kitty --class dropdown &
#thunar &

sleep 5

