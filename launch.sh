#!/bin/sh

# Terminate already running bar instances
pkill polybar

# DPI scaled parameters
export POLYBAR_DPI=$(echo - | awk "{ print 96 * ${GDK_SCALE:-1} }")
export POLYBAR_HEIGHT=$(echo - | awk "{ print 32 * ${GDK_SCALE:-1} }")
export POLYBAR_LINE_SIZE=$(echo - | awk "{ print 1.5 * ${GDK_SCALE:-1} }")
export POLYBAR_TRAY_PADDING=$(echo - | awk "{ print 4 * ${GDK_SCALE:-1} }")
export POLYBAR_TRAY_MAXSIZE=$(echo - | awk "{ print 16 * ${GDK_SCALE:-1} }")

# Find wlan interface
export WLAN_INTERFACE=$(ip link show | grep wlp | awk -F':' '{print $2}' | awk '{print $1}')

# Launch bar
polybar --reload main
