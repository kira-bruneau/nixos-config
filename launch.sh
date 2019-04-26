#!/bin/sh

# Terminate already running bar instances
pkill polybar

# Launch bar with matching hostname
polybar $(hostname)
