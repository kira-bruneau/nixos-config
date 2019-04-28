#!/bin/sh

# Terminate already running bar instances
pkill polybar

# Find wlan interface
export WLAN_INTERFACE=$(ip link show | grep wlp | awk -F':' '{print $2}' | awk '{print $1}')

# Launch bar
polybar --reload main
