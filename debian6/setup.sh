#!/bin/sh

sudo apt install git

cd ~
git clone https://github.com/evanlh/dotfiles

sudo apt install emacs
sudo apt install sway
sudo apt install zsh
sudo apt install pmbootstrap

rm ~/.gitconfig; ln -s ./dotfiles/dotgitconfig ~/.gitconfig

gsettings set org.gnome.desktop.interface enable-animations false

# foot terminal config
mkdir -p ~/.config/foot
ln -s ~/dotfiles/debian6/foot.ini ~/.config/foot

# emacs config
ln -s ~/dotfiles/dotemacs.el ~/.emacs.el
ln -s ~/dotfiles/my-emacs/ ~/.emacs.d/
ls ~/.emacs.d/

