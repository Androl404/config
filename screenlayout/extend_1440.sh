#!/bin/sh

xrandr --output eDP-1 --primary --mode 2560x1600 --pos 0x0 --rotate normal --output HDMI-1-0 --mode 2560x1440 --pos 2560x80 --rotate normal --output DP-1-0 --off --output DP-1-1 --off --output DP-1-2 --off --output DP-1-3 --off --output DP-1-4 --off --output DP-1-5 --off --output DP-1-6 --off
# For 144 Hz refresh rate
# xrandr --output HDMI-1-0 --mode 2560x1440 --refresh 144

# For performance mode
# xrandr --output eDP-1-1 --primary --mode 2560x1600 --pos 0x0 --rotate normal --output HDMI-0 --mode 2560x1440 --pos 2560x80 --rotate normal --output DP-0 --off --output DP-1 --off --output DP-2 --off --output DP-3 --off --output DP-4 --off --output DP-5 --off --output DP-6 --off

~/Documents/background.sh
