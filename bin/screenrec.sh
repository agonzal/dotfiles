#!/bin/bash


RECORDINGS_DIR=~/vids/recordings/

# --- Resolution ---
# >> Manually
# SCREEN_RESOLUTION=1920x1080
# >> Automatically
SCREEN_RESOLUTION="$(xwininfo -root | grep geometry | awk '{print $2}' | cut -d + -f1)"
# >> TODO select area to record with `slop`
# --- Audio ---
# Comment out both if you want to disable sound
# >> Pulse
AUDIO=" -f pulse -ac 2 -i default "
# >> Alsa
# AUDIO=" f alsa -ac 2 -i hw:0 "

# Notification icon path
REC_ICON_PATH=/home/electr0n/.icons/horizon-icons/16x16/actions/media-playback-start.svg
# --------------------------------------------

# Create directory if needed
if [ ! -d "$RECORDINGS_DIR" ]; then
    mkdir -p "$RECORDINGS_DIR"
fi

# Search for screen recording process
pid="$(ps -o pid,command ax | grep "ffmpeg" | grep "x11grab" | awk '{print $1}')"
if [ -z "$pid" ]; then
    TIMESTAMP="$(date +%Y.%m.%d-%H.%M.%S)"
    FILENAME=$RECORDINGS_DIR/$TIMESTAMP.screenrec.mp4

    notify-send -a "screenrec.sh" "Screen is being recorded." --urgency low -i $REC_ICON_PATH

    # --- Hardware decoding (for NVIDIA GPU) ---
    # Needs ffmpeg compiled with --enable-nvenc
    # ffmpeg $AUDIO -s $SCREEN_RESOLUTION -f x11grab -i :0.0 -c:v h264_nvenc -profile high444p -pixel_format yuv444p -preset default $FILENAME

    # --- Software decoding ---
    ffmpeg -f x11grab -s $SCREEN_RESOLUTION -i :0 -vcodec libx264 -preset medium -crf 22 -y $FILENAME

    notify-send "Screen recording over." --urgency low -i $REC_ICON_PATH
else
    # Stop recording
    kill $pid
fi
