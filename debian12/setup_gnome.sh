#!/bin/bash

# gnome config
gsettings set org.gnome.desktop.interface enable-animations false
gsettings set org.gnome.desktop.default-applications.terminal exec 'foot'

# gnome shell keybindings
gsettings set org.gnome.shell.keybindings toggle-overview '[]'
gsettings set org.gnome.shell.keybindings toggle-message-tray '[]'
gsettings set org.gnome.shell.keybindings shift-overview-down '[]'
gsettings set org.gnome.shell.keybindings shift-overview-up '[]'
gsettings set org.gnome.shell.keybindings show-screen-recording-ui "['<Ctrl><Shift><Alt>R', '<Super><Shift>5']"
gsettings set org.gnome.shell.keybindings show-screenshot-ui "['Print', '<Super><Shift>4']"

# gnome wm keybindings
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-down []
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-up []
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-last []
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-left "['<Control>Left']"
gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-right "['<Control>Right']"
gsettings set org.gnome.desktop.wm.keybindings maximize "[]"
gsettings set org.gnome.desktop.wm.keybindings toggle-maximized "['<Control><Alt>Space']"
gsettings set org.gnome.desktop.wm.keybindings unmaximize "[]"
gsettings set org.gnome.desktop.wm.keybindings maximize-horizontally []
gsettings set org.gnome.desktop.wm.keybindings maximize-vertically []
gsettings set org.gnome.desktop.wm.keybindings minimize []

gsettings set org.gnome.desktop.wm.keybindings move-to-corner-ne "['<Control><Alt>O']"
gsettings set org.gnome.desktop.wm.keybindings move-to-corner-nw "['<Control><Alt>U']"
gsettings set org.gnome.desktop.wm.keybindings move-to-corner-se "['<Control><Alt>period']"
gsettings set org.gnome.desktop.wm.keybindings move-to-corner-sw "['<Control><Alt>M']"
gsettings set org.gnome.mutter.keybindings toggle-tiled-left "['<Control><Alt>Left', '<Control><Alt>j']"
gsettings set org.gnome.mutter.keybindings toggle-tiled-right "['<Control><Alt>Right', '<Control><Alt>l']"
gsettings set org.gnome.desktop.wm.keybindings move-to-side-n "['<Control><Alt>i']"
gsettings set org.gnome.desktop.wm.keybindings move-to-side-s "['<Control><Alt>comma']"
gsettings set org.gnome.desktop.wm.keybindings move-to-side-e "[]"
gsettings set org.gnome.desktop.wm.keybindings move-to-side-w "[]"

# gnome keybindings rebind super-# to switch workspaces
# first clear out the switch-to-application bindings
for i in {1..9}; do
    gsettings set org.gnome.shell.keybindings switch-to-application-"$i" "[]";
done

for i in {1..9}; do
    gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-"$i" "['<Super>$i']";
done

# gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-1 "['<Super>1']"
# gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-2 "['<Super>2']"
# gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-3 "['<Super>3']"
# gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-4 "['<Super>4']"
# gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-5 "['<Super>5']"
# gsettings set org.gnome.desktop.wm.keybindings switch-to-workspace-6 "['<Super>6']"
