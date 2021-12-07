#!/bin/sh 
# note: notify-send is required, see libnotify-bin

# XXX do not notify if notification source has focus
icon=~/.irssi.png
delay="2000"

read line
summary="$line"
read line
msg="$line"
read line

if [ "$line" = "" ] && [ "$summary" != "" ]; then
    # Change the icon
    #[ -x "$(which notify-send)" ] && notify-send -i gtk-dialog-info -t "$delay" -- "$summary" "$msg"
    [ -x "$(which notify-send)" ] && notify-send -a irssi -i $icon -t "$delay" -- "$summary" "$msg"
fi

