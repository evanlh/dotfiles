#!/bin/bash

cd $HOME
# setup preferred directories
mkdir -p $HOME/bin
mkdir -p $HOME/code
mkdir -p $HOME/scratch

sudo apt install git

# clone dotfiles and setup core directories
if [ ! -d "$HOME/dotfiles" ] ; then
    git clone git@github.com:evanlh/dotfiles.git
fi

if [ ! -d "$HOME/writing" ] ; then
    git clone git@github.com:evanlh/writing.git
fi

for repo in cheatsheets graph cl-masterblaster ts-datastructures; do
    cd $HOME/code/
    git clone git@github.com:evanlh/${repo}.git
done
cd $HOME

if [ ! -d "$HOME/.ssh" ] ; then
    ssh-keygen
fi

# TODO need to manually install backports, below:
# apt edit-sources
# deb http://deb.debian.org/debian bookworm-backports main contrib non-free non-free-firmware
# apt update

# install favorite packages

# essentials
# sudo apt install emacs-gtk/bookworm-backports
# sudo apt install emacs
sudo apt install silversearcher-ag
sudo apt install zsh
sudo apt install curl
sudo apt install wget
sudo apt install pandoc
sudo apt install plocate

# current wm / term favorites
sudo apt install sway
sudo apt install foot
sudo apt install wl-clipboard
sudo apt install fonts-powerline
sudo apt install powerline

# programming languages
sudo apt install clojure
sudo apt install guile-3.0
sudo apt install sbcl
sudo apt install clang

# sudo apt install python3
# sudo apt install virtualenv
# TODO pip?

# misc
# sudo apt install pmbootstrap
# TODO
# babashka

# refresh font cache
fc-cache

# install out-of-distro packages

# nvm
if [ ! -d "$HOME/.nvm" ] ; then
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.1/install.sh | bash
fi
# nvm install 20
# npm install -g typescript-language-server typescript

# rust
if [ ! -d "$HOME/.cargo" ] ; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi

# bash config
ln -s ./dotfiles/dotbashrc ~/.bashrc
ln -s ./dotfiles/dotbashrc_linux ~/.bashrc_linux

# git config
rm ~/.gitconfig; ln -s ./dotfiles/dotgitconfig ~/.gitconfig
rm ~/.gitignore; ln -s ./dotfiles/dotgitignore_global ~/.gitignore
git config --global core.excludesFile '~/.gitignore'

# foot terminal config
mkdir -p ~/.config/foot
ln -s ~/dotfiles/debian12/foot.ini ~/.config/foot

# emacs config
mkdir -p ~/.emacs.d/
ln -s ~/dotfiles/dotemacs.el ~/.emacs.el
ln -s ~/dotfiles/my-emacs/ ~/.emacs.d/

# sway config
ln -s ~/dotfiles/debian12/sway ~/.config/sway

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

# TODO
# - default fonts - not sure Source Code Pro is configured, need to increase default size
# - Bash prompt - Ideas - Current time - Location of logged in IP as flag emoji - Process exit code (powerline) - Powerline arrows - Mushroom/magic for not/root - username & machine
# - Emacs config
#   - Want / need to get typescript-ts-mode working (currently blocked on not having aarch64 treesitter ... or?). Ideal ts modes for: rust, swift.
#   - Confirm sbcl & clojure work out of the box
#   - General housecleaning
#   - migrate to modern use-package?
#
# - Sway
#   - Love the launcher & tiling modes :)
#   - Bug with cursor placement needs new wlroots https://gitlab.freedesktop.org/wlroots/wlroots/-/merge_requests/4621
#   - Color scheme to match shell
#   - Swaybar bells and whistles
