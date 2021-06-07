#!/usr/bin/env zsh

## invoke autolock every 2 mins. Use Win + Z to invoke inhibitor to stop this behavior. 

xset s 500 &
xautolock -time 5 -locker "betterlockscreen -l blur" -notify 10 -notifier "notify-send 'Locker' 'Locking screen in 10 seconds'" 


#-killtime 5 -killer "ssystemctl suspend"
#xautolock -time 2 -locker "systemctl suspend"


