#!/bin/bash

sleep 10

while true; do
    # -F for fullscreen
    # -r for recursive
    # -z for randomize
    # --no-fehbg avoids creating a .feh file
    # --bg-max preserves aspect ratio and scales the image to the size that fits the screen
feh --no-fehbg -F -r --bg-max -z /home/alex/Pictures/Wallpaper/ && sleep 30m || exit

done
