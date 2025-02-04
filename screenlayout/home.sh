#!/bin/sh
# xrandr --output eDP-1 --primary --mode 2560x1600 --pos 0x0 --rotate normal --output HDMI-1-0 --mode 3440x1440 --pos 2560x80 --rotate normal --output DP-1-0 --off --output DP-1-1 --off --output DP-1-2 --off --output DP-1-3 --off --output DP-1-4 --off --output DP-1-5 --off --output DP-1-6 --off

# For refresh rate at 50 FPS because of NVIDIA drivers
# Needs to be reverted when NVIDIA drivers will be fixed
xrandr --output eDP-1 --primary --mode 2560x1600 --pos 0x0 --rotate normal --output HDMI-1-0 --mode 3440x1440 --pos 2560x80 --rotate normal --refresh 50 --output DP-1-0 --off --output DP-1-1 --off --output DP-1-2 --off --output DP-1-3 --off --output DP-1-4 --off --output DP-1-5 --off --output DP-1-6 --off

# For performance mode (with refresh rate at 50)
# xrandr --output eDP-1-1 --primary --mode 2560x1600 --pos 0x0 --rotate normal --output HDMI-0 --mode 3440x1440 --pos 2560x80 --rotate normal --refresh 50 --output DP-0 --off --output DP-1 --off --output DP-2 --off --output DP-3 --off --output DP-4 --off --output DP-5 --off --output DP-6 --off

~/Documents/background.sh
