#!/usr/bin/env bash

## Author  : Aditya Shakya
## Mail    : adi1090x@gmail.com
## Github  : @adi1090x
## Twitter : @adi1090x
## Modified for my usage. 

# >> Created and tested on : rofi 1.6.0-1

theme="style"
#theme="dracula"

dir="$HOME/.config/rofi/launcher"

rofi -no-lazy-grab -show drun \
-modi drun,window,ssh,file-browser,":greenclip print" \
-theme $dir/"$theme" -display-file-browser ﱮ \
-drun-icon-theme "Black-Frost-Suru"

