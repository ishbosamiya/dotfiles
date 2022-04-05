# Ish's Dotfiles

Using GNU Stow `sudo apt install stow` to automatically link the dotfiles in the required directories.

## For VNC

Cannot use stow, since the files must go to `/usr/local/bin` and
`/etc/systemd/system/`.

Run the command `cd vnc && sudo ./install_files.sh` for installation.

Read the output of the script to know what more to install and
configure.

## For Alacritty

Install [Alacritty](https://github.com/alacritty/alacritty) by compiling it.

Need to update the default terminal to be Alacritty by running

``` shell
gsettings set org.gnome.desktop.default-applications.terminal exec alacritty
```
This sadly doesn't update the nautilus context menu to open the
terminal as Alacritty so manually install (installing through pip doesn't work out properly)
[nautilus-open-any-terminal](https://github.com/Stunkymonkey/nautilus-open-any-terminal).

Then run

``` shell
gsettings set com.github.stunkymonkey.nautilus-open-any-terminal terminal alacritty
```
