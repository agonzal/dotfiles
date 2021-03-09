#!/usr/bin/env zsh
xset s 500 &
#xautolock -time 5 -locker "betterlockscreen -l" -notify 10 -nodtifier "notify-send 'Locker' 'Locking screen in 10 seconds'" -killtime 5 -killer "systemd suspend"
xautolock -time 2 -locker "systemctl suspend"


