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

To update the nautilus context menu to open the terminal as Alacritty,
manually install (installing through pip doesn't work out properly)
[nautilus-open-any-terminal](https://github.com/Stunkymonkey/nautilus-open-any-terminal).

Then run

``` shell
gsettings set com.github.stunkymonkey.nautilus-open-any-terminal terminal alacritty
```

Gnome no longer has any indication of default terminal emulator thus
there is no way to set a default terminal. The best way is to override
the terminal opening shortcut to open `Alacritty` instead of
`gnome-terminal`.

## For Nu Shell

Install [Nu](https://www.nushell.sh/) by compiling it.

The prompt used for `nu` is `oh-my-posh` and depends on a custom
theme. This requires [oh-my-posh](https://ohmyposh.dev/) to be
installed.

## For oh-my-posh

[Installation
instructions](https://ohmyposh.dev/docs/installation/linux) provided
by `oh-my-posh` installs it globally. Not really a good idea so install it using

``` shell
wget https://github.com/JanDeDobbeleer/oh-my-posh/releases/latest/download/posh-linux-amd64 -O ~/.local/bin/oh-my-posh
chmod +x ~/.local/bin/oh-my-posh
```

For "installing" the themes, the install instructions provided on the
website work well.

### Note

This requires at least one [nerd-font](https://www.nerdfonts.com/) to
be installed.
