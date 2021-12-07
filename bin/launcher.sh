#!/usr/bin/env bash
# shell script to launch rofi w/ clipboard support 
# github.com/agonzal/dotfiles 

theme="style"
#theme="dracula"

dir="$HOME/.config/rofi/launcher"

rofi -no-lazy-grab -show drun \
-modi window,drun,run,✗:~/.config/rofi/scripts/kill.sh,filebrowser," :/home/electr0n/bin/greenclip print" \
-theme $dir/"$theme" -display-filebrowser "🗁  " \
-drun-icon-theme "horizon-icons"

