#!/bin/bash
xsetwacom set "Tablet Monitor stylus" Button 2 3
xsetwacom set "Tablet Monitor stylus" Button 3 2
xsetwacom set "Tablet Monitor stylus" MapToOutput HEAD-2
xsetwacom set "Tablet Monitor Pad pad" button 9 "key +ctrl z -ctrl"
xsetwacom set "Tablet Monitor Pad pad" button 1 "key b"
xsetwacom set "Tablet Monitor Pad pad" button 2 "key l"
xsetwacom set "Tablet Monitor Pad pad" button 3 "key e"
xsetwacom set "Tablet Monitor Pad pad" button 8 "key +ctrl +shift z -shift -ctrl"
xsetwacom --set "Tablet Monitor Pad pad" button 13 "key +space"
xsetwacom --set "Tablet Monitor Pad pad" button 14 "key +ctrl"
