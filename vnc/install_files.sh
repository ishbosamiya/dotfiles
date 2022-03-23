#! /bin/bash

SCRIPT_LOCATION="/usr/local/bin/"
SCRIPT_FILE_LOCATION="x11vnc_startup.sh"
SERVICE_LOCATION="/etc/systemd/system/"
SERVICE_FILE_LOCATION="x11vnc_server.service"

echo "Requires \"x11vnc\" and a vnc password to be generated and stored at $HOME/.vnc/passwd (use \"x11vnc -storepasswd\")."
echo "The display configuration might be different, so might need to be updated in x11vnc_startup.sh"

echo "cp" $SCRIPT_FILE_LOCATION $SCRIPT_LOCATION
cp $SCRIPT_FILE_LOCATION $SCRIPT_LOCATION

echo "cp" $SERVICE_FILE_LOCATION $SERVICE_LOCATION
cp $SERVICE_FILE_LOCATION $SERVICE_LOCATION

echo "restarting systemctl daemon"
systemctl daemon-reload

echo "starting x11vnc_server.service"
systemctl start x11vnc_server.service

echo "making x11vnc_server.service auto restart"
systemctl enable x11vnc_server.service
