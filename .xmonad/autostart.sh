#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

POLYBAR=/home/electr0n/.config/polybar/config.ini 

xrandr --output DP1 --off --output DP2 --off --output DP3 --mode 2880x1620 --pos 0x0 --rotate normal --output HDMI1 --off --output HDMI2 --off --output HDMI3 --off --output VIRTUAL1 --off

#numlockx on &
run blueman-applet &
run picom -fb &
run volumeicon 

# caps as esc or control when used with another key. 
run setxkbmap -option ctrl:nocaps 
run xcape -e 'Control_L=Escape'
run xcape -e "Super_L=Super_L|o"

run /usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
#run pa-applet &
run nitrogen --restore &
#feh --bg-center ~/wallpapers/spaceman-oned.jpg &
run greenclip daemon &
run dunst &
run polybar xmonad -r -c $POLYBAR &


#kitty --class dropdown &
#thunar &

sleep 5

