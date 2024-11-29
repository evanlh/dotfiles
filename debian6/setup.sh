#!/bin/sh

cd $HOME
sudo apt install git

# clone dotfiles and setup core directories
if [ ! -d "$HOME/dotfiles" ] ; then
    git clone git@github.com:evanlh/dotfiles.git
fi

if [ ! -d "$HOME/writing" ] ; then
    git clone git@github.com:evanlh/writing.git
fi

if [ ! -d "$HOME/.ssh" ] ; then
    ssh-keygen
fi

# install favorite packages
sudo apt install emacs
sudo apt install sway
sudo apt install foot
sudo apt install zsh
sudo apt install silversearcher-ag
sudo apt install pmbootstrap
sudo apt install clojure
sudo apt install guile-3.0
sudo apt install sbcl
sudo apt install wl-clipboard

# git config
rm ~/.gitconfig; ln -s ./dotfiles/dotgitconfig ~/.gitconfig
rm ~/.gitignore; ln -s ./dotfiles/dotgitignore_global ~/.gitignore
git config --global core.excludesFile '~/.gitignore'

# gnome config
gsettings set org.gnome.desktop.interface enable-animations false

gsettings set org.gnome.shell.keybindings toggle-overview '[]'
gsettings set org.gnome.shell.keybindings toggle-message-tray '[]'
gsettings set org.gnome.shell.keybindings shift-overview-down '[]'
gsettings set org.gnome.shell.keybindings shift-overview-up '[]'

# foot terminal config
mkdir -p ~/.config/foot
ln -s ~/dotfiles/debian6/foot.ini ~/.config/foot

# emacs config
mkdir -p ~/.emacs.d/
ln -s ~/dotfiles/dotemacs.el ~/.emacs.el
ln -s ~/dotfiles/my-emacs/ ~/.emacs.d/

