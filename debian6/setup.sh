#!/bin/sh
#
cd $HOME
sudo apt install git

if [ ! -d "$HOME/dotfiles" ] ; then
    git clone https://github.com/evanlh/dotfiles
fi

if [ ! -d "$HOME/.ssh" ] ; then
    ssh-keygen
fi

sudo apt install emacs
sudo apt install sway
sudo apt install foot
sudo apt install zsh
sudo apt install silversearcher-ag
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

