#!/usr/bin/env bash

## Author  : Aditya Shakya
## Mail    : adi1090x@gmail.com
## Github  : @adi1090x
## Twitter : @adi1090x
## Modified for my usage. 

# >> Created and tested on : rofi 1.6.0-1

#theme="style"
theme="dracula"

dir="$HOME/.config/rofi/launcher"

rofi -no-lazy-grab -show ssh \
-modi run,drun,window,ssh \
-theme $dir/"$theme" -drun-icon-theme "candy-icons"

