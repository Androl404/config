#!/bin/sh

# TODO: Make a function which creates the simlink
# Because the actual system is not easily portable

set -e
SCRIPT_DIR="$( cd "$( dirname "$BASH_SOURCE[0]" )" && pwd )"

# For gVim (Vim with graphical interface)
ln -sf "$SCRIPT_DIR/vim/.gvimrc" ~/.gvimrc
# For tmux
ln -sf "$SCRIPT_DIR/tmux/.tmux.conf" ~/.tmux.conf

# Verify if .config directory exists
mkdir -p ~/.config
mkdir -p ~/.config/nvim/
mkdir -p ~/.config/alacritty/
mkdir -p ~/.config/kitty/
mkdir -p ~/.config/i3/
mkdir -p ~/.config/rofi/
mkdir -p ~/.config/rofi/themes/
mkdir -p ~/.config/hyprland/
mkdir -p ~/.emacs.d/

# For NeoVim
ln -sf "$SCRIPT_DIR/neovim/init.lua" ~/.config/nvim/init.lua

# For Alacritty
ln -sf "$SCRIPT_DIR/alacritty/alacritty.toml" ~/.config/alacritty/alacritty.toml

# For Kitty
ln -sf "$SCRIPT_DIR/kitty/kitty.conf" ~/.config/kitty/kitty.conf

# For i3wm
ln -sf "$SCRIPT_DIR/i3wm/config" ~/.config/i3/config
ln -sf "$SCRIPT_DIR/i3wm/.i3status.conf" ~/ # .config/i3/config

# For Emacs
ln -sf "$SCRIPT_DIR/emacs/rc.el" ~/.emacs.d/rc.el
ln -sf "$SCRIPT_DIR/emacs/init.el" ~/.emacs.d/init.el
ln -sf "$SCRIPT_DIR/emacs/custom.el" ~/.emacs.d/custom.el
ln -sf "$SCRIPT_DIR/emacs/org.el" ~/.emacs.d/org.el
ln -sf "$SCRIPT_DIR/emacs/org-style.css" ~/.emacs.d/org-style.css

# For Rofi
ln -sf "$SCRIPT_DIR/rofi/config.rasi" ~/.config/rofi/config.rasi
ln -sf "$SCRIPT_DIR/rofi/themes/dtos-center-new.rasi" ~/.config/rofi/themes/dtos-center-new.rasi
ln -sf "$SCRIPT_DIR/rofi/themes/dtos-center.rasi" ~/.config/rofi/themes/dtos-center.rasi
ln -sf "$SCRIPT_DIR/rofi/themes/dtos-dmenu.rasi" ~/.config/rofi/themes/dtos-dmenu.rasi

# For hyprland
ln -sf "$SCRIPT_DIR/hyprland/hyprland.conf" ~/.config/hyprland/hyprland.conf

