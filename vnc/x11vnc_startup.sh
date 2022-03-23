#! /bin/bash

# Start VNC server for :1 display in background
# TODO: figure out display properly, don't just assume :1

## Set path to binary file
VNC_BIN=/usr/bin/x11vnc

## Set parameters
PARAMS="-display :1 -rfbauth $HOME/.vnc/passwd -forever -shared"

## Launch VNC server
echo "launching vnc server"
echo $VNC_BIN $PARAMS
($VNC_BIN $PARAMS) &

# Provide clean exit code for the service
exit 0
