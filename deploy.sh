#!/usr/bin/env sh
set -e

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
ACTION="$1"       # install | uninstall
shift             # Remaining arguments are program names (optional)

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
vim      vim/.gvimrc                      ~/.gvimrc
tmux     tmux/.tmux.conf                  ~/.tmux.conf
nvim     neovim/init.lua                  ~/.config/nvim/init.lua
alacritty alacritty/alacritty.toml        ~/.config/alacritty/alacritty.toml
kitty    kitty/kitty.conf                 ~/.config/kitty/kitty.conf
i3       i3wm/config                      ~/.config/i3/config
i3       i3wm/.i3status.conf              ~/.i3status.conf
hypr     hyprland/hyprland.conf           ~/.config/hyprland/hyprland.conf
sway     sway/config                       ~/.config/sway/config
waybar   waybar/config                     ~/.config/waybar/config
waybar   waybar/style.css                  ~/.config/waybar/style.css
rofi     rofi/config.rasi                  ~/.config/rofi/config.rasi
rofi     rofi/leave.sh                     ~/.config/rofi/leave.sh
rofi     rofi/themes/dtos-center-new.rasi  ~/.config/rofi/themes/dtos-center-new.rasi
rofi     rofi/themes/dtos-center.rasi      ~/.config/rofi/themes/dtos-center.rasi
rofi     rofi/themes/dtos-dmenu.rasi       ~/.config/rofi/themes/dtos-dmenu.rasi
emacs    emacs/init.el                     ~/.emacs.d/init.el
emacs    emacs/rc.el                       ~/.emacs.d/rc.el
emacs    emacs/custom.el                   ~/.emacs.d/custom.el
emacs    emacs/org.el                      ~/.emacs.d/org.el
emacs    emacs/magit-windows.el            ~/.emacs.d/magit-windows.el
emacs    emacs/w32-browser.el              ~/.emacs.d/w32-browser.el
emacs    emacs/org-style.css               ~/.emacs.d/org-style.css
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
