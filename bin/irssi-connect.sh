#!/bin/sh

# If we get an argument, use it for ssh port, otherwise use default of 22
if [ -n "$1" ]
then
    port=$1
else
    port=22
fi

host="hunt.revrse.sh"
notify="$HOME/bin/irssi-notif.sh"

set -e

socat -u TCP4-LISTEN:12000,reuseaddr,fork,bind=127.0.0.1 EXEC:$notify &

# If you only have one remote screen session
#autossh $host -p $port -R 12000:localhost:12000 -t 'screen -r -D'

# Attaches to tmux session 
ssh $host -p $port -R 12000:localhost:12000 -t 'tmux attach'

kill %1
