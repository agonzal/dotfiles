#!/usr/bin/env zsh

## 
## Use xautolock to invoke betterlockscreen every 5 mins. Use Super + Shift + A to disable this feature.  
ICON=~/.icons/horizon-icons/32x32/apps/system-lock-screen.svg

#xset s 320 &
xautolock -time 5 -locker "lockscreen" -notify 10 -notifier "notify-send -a 'autolock.sh' -i $ICON 'Locker: i3lock' 'Locking screen in 10 seconds'" 


#-killtime 5 -killer "ssystemctl suspend"
#xautolock -time 2 -locker "systemctl suspend"


