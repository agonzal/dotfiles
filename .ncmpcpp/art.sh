#!/usr/bin/env sh

#-------------------------------#
# Generate current song cover   #
# ffmpeg version                #
#-------------------------------#

# Path to music directory
MUSIC_DIR="$HOME/Music"
# Path to output cover
COVER="/tmp/cover.png"
COVER_NOTIFICATION="/tmp/cover_notification.png"
# Size of cover
COVER_SIZE=297
# Size in pixel of borders to crop out
CROP_BORDER=20
# Radius or rounded borders
BORDER_RADIUS=10

ffmpeg_cover() {
    ffmpeg -loglevel 0 -y -i "$1" -vf "crop=min(in_w-$CROP_BORDER\,in_h-$CROP_BORDER):out_w,scale=-2:$COVER_SIZE" "$COVER"
}

rounded_cover() {
    convert -quiet "$COVER" \
     \( +clone  -alpha extract \
        -draw "fill black polygon 0,0 0,$BORDER_RADIUS $BORDER_RADIUS,0 fill white circle $BORDER_RADIUS,$BORDER_RADIUS $BORDER_RADIUS,0" \
        \( +clone -flip \) -compose Multiply -composite \
        \( +clone -flop \) -compose Multiply -composite \
     \) -alpha off -compose CopyOpacity -composite "$COVER"
}

#fallback_find_cover() {
#    album=$(dirname "$file")
#    album_cover="$(find "$album" -type d -exec find {} -maxdepth 1 -type f -iregex ".*\(covers?\|folders?\|artworks?\|fronts?\|scans?\).*[.]\(jpe?g\|png\|gif\|bmp\)" \;)"
#    [ -z "$album_cover" ] && album_cover="$(find "$album" -type d -exec find {} -maxdepth 1 -type f -iregex ".*[.]\(jpe?g\|png\|gif\|bmp\)" \;)"
#    [ -z "$album_cover" ] && album_cover="$(find "${album%/*}" -type d -exec find {} -maxdepth 1 -type f -iregex ".*\(covers?\|folders?\|artworks?\|fronts?\|scans?\|booklets?\).*[.]\(jpe?g\|png\|gif\|bmp\)" \;)"
#    album_cover="$(echo "$album_cover" | grep -iv '\(back\|cd\)\.' | head -n1)"
#}

mpris_album_art() {
    playerctl metadata mpris:artUrl | sed 's#file://##'
    # mpris_player_control -t | grep 'artUrl' | cut -f 3 -d '|' | sed 's#file://##'
}

notification() {
    convert "$COVER" -resize 144x144 "$COVER_NOTIFICATION"
    notify-send -i "$COVER_NOTIFICATION" "$(playerctl metadata --format '{{title}} {{album}}')"
}

main() {
    # file="$MUSIC_DIR/$(mpc --format %file% current)"

    # [ -n "$file" ] && ffmpeg_cover "$file" && rounded_cover ||
    #    fallback_find_cover && ffmpeg_cover "$album_cover" && rounded_cover
    [ ! -x "$(which playerctl)" ] && echo "Install playerctl and mpDris2 for this script to work."

    ffmpeg_cover "$(mpris_album_art)" && rounded_cover
    notification
}

main

