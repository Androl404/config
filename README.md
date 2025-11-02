# My personal configurations files

Here are the programs which the config is stored in this repo:
* NeoVim
* Vim
* Emacs
* Tmux
* Alacritty
* Kitty
* i3wm
* Rofi
* Hyprland
* Waybar

The font [Iosevka Nerd Font](https://github.com/ryanoasis/nerd-fonts/releases/download/v3.3.0/Iosevka.zip) are required in order for the config to work properly. Multiple others font are necessary for org mode.

## Usage

Install everything:
``` bash
./deploy.sh install
```

Install only some tools:
``` bash
./deploy.sh install emacs rofi nvim
```

Remove (uninstall) configs for a specific tool:
``` bash
./deploy.sh uninstall rofi
```

Uninstall everything:
``` bash
./deploy.sh uninstall all
```
