#!/usr/bin/sh

# HiDPI
# https://wiki.archlinux.org/index.php/HiDPI

# Set DPI for IntelliJ IDEA
xrandr --dpi 192

# Merge in HiDPI specific X properties
xrdb -load ~/.Xresources
xrdb -merge ~/.Xresources-HiDPI

# GDK 3 virtual pixel scaling
export GDK_SCALE=2
export GDK_DPI_SCALE=0.5

# Qt 5 virtual pixel scaling
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_SCREEN_SCALE_FACTORS=2

# winit virtual pixel scaling
export WINIT_HIDPI_FACTOR=2

# Use GTK 3 settings in Qt 5
# https://wiki.archlinux.org/index.php/Uniform_look_for_Qt_and_GTK_applications
export QT_QPA_PLATFORMTHEME=gtk3

# Improve appearance of Java applications
# https://wiki.archlinux.org/index.php/Java#Tips_and_tricks
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.crossplatformlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

# Touchscreen support on Firefox
export MOZ_USE_XINPUT2=1

# Set keyboard layouts
setxkbmap -layout 'us,us' -variant 'colemak,' -option 'grp:win_space_toggle,caps:backspace'

# Start i3
exec i3
