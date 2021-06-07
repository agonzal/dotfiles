#!/usr/bin/env bash

#-------------------------------#
# Display current cover         #
#-------------------------------#

source "`ueberzug library`"

COVER="/tmp/cover.jpg"
X_PADDING=0
Y_PADDING=0

function add_cover() {
    ImageLayer::add [identifier]="cover" [x]="$X_PADDING" [y]="$Y_PADDING" [path]="$COVER"
}

function remove_cover() {
    ImageLayer::remove [identifier]="cover"
}

function you_wait() {
    while inotifywait -q -q -e close_write "$COVER"; do
        add_cover
    done
}

clear

ImageLayer 0< <(
    add_cover
    you_wait
)

