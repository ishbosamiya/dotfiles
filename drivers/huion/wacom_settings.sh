#!/bin/bash
xsetwacom set "Tablet Monitor stylus" Button 2 3
xsetwacom set "Tablet Monitor stylus" Button 3 2

# TODO: need to make setting display more generic, it should be
# possible to gather which is the tablet monitor and thus get it's
# mapping to be able to set the the stylus to be bound to that region

# # If the displays are
# #
# # 1 2
# #   3
# #
# # Map to 3.
# xsetwacom --set "Tablet Monitor stylus" MapToOutput "1920x1080+1920+1080"

# If the displays are
# 1
# 2
#
# Map to 2.
xsetwacom --set "Tablet Monitor stylus" MapToOutput "1920x1080+0+1080"

xsetwacom set "Tablet Monitor Pad pad" button 9 "key +ctrl z -ctrl"
xsetwacom set "Tablet Monitor Pad pad" button 1 "key b"
xsetwacom set "Tablet Monitor Pad pad" button 2 "key l"
xsetwacom set "Tablet Monitor Pad pad" button 3 "key e"
xsetwacom set "Tablet Monitor Pad pad" button 8 "key +ctrl +shift z -shift -ctrl"
xsetwacom --set "Tablet Monitor Pad pad" button 13 "key +space"
xsetwacom --set "Tablet Monitor Pad pad" button 14 "key +ctrl"
