# Ish's Dotfiles

Using GNU Stow `sudo apt install stow` to automatically link the dotfiles in the required directories.

## For VNC

Cannot use stow, since the files must go to `/usr/local/bin` and
`/etc/systemd/system/`.

Run the command `cd vnc && sudo ./install_files.sh` for installation.

Read the output of the script to know what more to install and
configure.
