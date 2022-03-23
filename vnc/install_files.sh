#! /bin/bash

SCRIPT_LOCATION="/usr/local/bin/"
SCRIPT_FILE_LOCATION="x0vnc.sh"
SERVICE_LOCATION="/etc/systemd/system/"
SERVICE_FILE_LOCATION="x0vncserver.service"

echo "Requires tigervnc's x0vncserver installed at /usr/bin/x0vncserver and a vnc password to be generated and stored at $HOME/.vnc/passwd (use vncpasswd)."
echo "The display configuration might be different, so might need to be updated in x0vnc.sh"

echo "cp" $SCRIPT_FILE_LOCATION $SCRIPT_LOCATION
cp $SCRIPT_FILE_LOCATION $SCRIPT_LOCATION

echo "cp" $SERVICE_FILE_LOCATION $SERVICE_LOCATION
cp $SERVICE_FILE_LOCATION $SERVICE_LOCATION

echo "restarting systemctl daemon"
systemctl daemon-reload

echo "starting x0vncserver.service"
systemctl start x0vncserver.service

echo "making x0vncserver.service auto restart"
systemctl enable x0vncserver.service
