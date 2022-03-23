#! /bin/bash

# Start VNC server for :1 display in background
# TODO: figure out display properly, don't just assume :1

## Set path to binary file
VNC_BIN=/usr/bin/x0vncserver

## Set parameters
PARAMS="-display :1 -PasswordFile $HOME/.vnc/passwd"
if [[ -f /etc/vnc.conf ]];
then
    ## Launch VNC server
    echo "launching vnc server"
    echo $VNC_BIN $PARAMS
    ($VNC_BIN $PARAMS) &
else
    ## Add parameters
    PARAMS+=" --I-KNOW-THIS-IS-INSECURE"

    ## Launch VNC server
    $VNC_BIN $PARAMS
fi

# Provide clean exit code for the service
exit 0
