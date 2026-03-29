#!/usr/bin/env sh
set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
ACTION="$1" # install | uninstall
shift       # Remaining arguments are program names (optional)

# --- Helper functions --------------------------------------------------------

install_link() {
    src="$SCRIPT_DIR/$1"
    dest="$2"
    mkdir -p "$(dirname "$dest")"
    ln -sf "$src" "$dest"
    printf "➕ Linked %s → %s\n" "$src" "$dest"
}

remove_link() {
    dest="$2"
    if [ -L "$dest" ]; then
        rm "$dest"
        printf "Removed symlink %s\n" "$dest"
    fi
}

# --- Config table ------------------------------------------------------------
# PROGRAM | source relative to repo | target absolute path

CONFIG="
vim      vim/.gvimrc                      $HOME/.gvimrc
tmux     tmux/.tmux.conf                  $HOME/.tmux.conf
nvim     neovim/init.lua                  $HOME/.config/nvim/init.lua
alacritty alacritty/alacritty.toml        $HOME/.config/alacritty/alacritty.toml
kitty    kitty/kitty.conf                 $HOME/.config/kitty/kitty.conf
kitty    kitty/current-theme.conf         $HOME/.config/kitty/current-theme.conf
i3       i3wm/config                      $HOME/.config/i3/config
i3       i3wm/.i3status.conf              $HOME/.i3status.conf
hypr     hyprland/hyprland.conf           $HOME/.config/hyprland/hyprland.conf
sway     sway/config                       $HOME/.config/sway/config
waybar   waybar/config                     $HOME/.config/waybar/config
waybar   waybar/style.css                  $HOME/.config/waybar/style.css
rofi     rofi/config.rasi                  $HOME/.config/rofi/config.rasi
rofi     rofi/leave.sh                     $HOME/.config/rofi/leave.sh
rofi     rofi/themes/dtos-center-new.rasi  $HOME/.config/rofi/themes/dtos-center-new.rasi
rofi     rofi/themes/dtos-center.rasi      $HOME/.config/rofi/themes/dtos-center.rasi
rofi     rofi/themes/dtos-dmenu.rasi       $HOME/.config/rofi/themes/dtos-dmenu.rasi
emacs    emacs/init.el                     $HOME/.emacs.d/init.el
emacs    emacs/rc.el                       $HOME/.emacs.d/rc.el
emacs    emacs/custom.el                   $HOME/.emacs.d/custom.el
emacs    emacs/org.el                      $HOME/.emacs.d/org.el
emacs    emacs/magit-windows.el            $HOME/.emacs.d/magit-windows.el
emacs    emacs/w32-browser.el              $HOME/.emacs.d/w32-browser.el
emacs    emacs/org-style.css               $HOME/.emacs.d/org-style.css
"

# --- Program selection --------------------------------------------------------
if [ $# -eq 0 ]; then
    PROGRAMS="all"
else
    PROGRAMS="$@"
fi

install_or_remove() {
    program="$1"
    action="$2"

    printf "\n== %s %s ==\n" "$action" "$program"

    echo "$CONFIG" | while read -r prog src dest; do
        [ -z "$prog" ] && continue

        if [ "$program" = "all" ] || [ "$program" = "$prog" ]; then
            case $action in
            install) install_link "$src" "$dest" ;;
            uninstall) remove_link "$src" "$dest" ;;
            esac
        fi
    done
}

# --- Dispatch ----------------------------------------------------------------

for p in $PROGRAMS; do
    install_or_remove "$p" "$ACTION"
done

printf "\nDone.\n"
