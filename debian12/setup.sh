#!/bin/bash

cd $HOME
# setup preferred directories
mkdir -p $HOME/bin
mkdir -p $HOME/code
mkdir -p $HOME/scratch

sudo apt install git

if [ ! -d "$HOME/.ssh" ] ; then
    ssh-keygen
fi

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

#TODO
# if ! `fc-list|grep "Source"`; then $HOME/dotfiles/debian12/install_fonts.sh; fi

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
sudo apt install libportaudio2
sudo apt install vim

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

# refresh font cache
fc-cache

# Install configs............
# bash config
ln -fs ./dotfiles/dotbashrc ~/.bashrc
ln -fs ./dotfiles/dotbashrc_linux ~/.bashrc_linux

# git config
rm ~/.gitconfig; ln -s ./dotfiles/dotgitconfig ~/.gitconfig
rm ~/.gitignore; ln -s ./dotfiles/dotgitignore_global ~/.gitignore
git config --global core.excludesFile '~/.gitignore'

# foot terminal config
mkdir -p ~/.config/foot
ln -sf ~/dotfiles/debian12/foot.ini ~/.config/foot/foot.ini

# emacs config
mkdir -p ~/.emacs.d/
ln -sf ~/dotfiles/dotemacs.el ~/.emacs.el
ln -sf ~/dotfiles/my-emacs/ ~/.emacs.d/

# sway config
ln -sf ~/dotfiles/debian12/sway ~/.config/sway

# sbcl
ln -sf ~/dotfiles/dotsbclrc ~/.sbclrc

# powerline
ln -sf ~/dotfiles/dotpowerline ~/.config/powerline

# Install out-of-distro packages...........
# nvm
if [ ! -d "$HOME/.nvm" ] ; then
  curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.1/install.sh | bash
fi

nvm install 20
npm install -g typescript-language-server typescript

# rust
if [ ! -d "$HOME/.cargo" ] ; then
  curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi

if [ ! -f "$HOME/quicklisp.lisp" ] ; then
  curl -O https://beta.quicklisp.org/quicklisp.lisp
  curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
  gpg --verify quicklisp.lisp.asc quicklisp.lisp
  sbcl --load quicklisp.lisp
fi

# babashka
if [ ! -f "$HOME/bin/bb" ] ; then
  cd $HOME/bin
  wget https://github.com/babashka/babashka/releases/download/v1.12.195/babashka-1.12.195-linux-aarch64-static.tar.gz
  tar -xzvf babashka-1.12.195-linux-aarch64-static.tar.gz
  rm babashka-1.12.195-linux-aarch64-static.tar.gz
  cd $HOME
fi

# flatpak
sudo apt install flatpak
sudo apt install gnome-software-plugin-flatpak
flatpak remote-add --if-not-exists flathub https://dl.flathub.org/repo/flathub.flatpakrepo

~/dotfiles/debian12/setup_gnome.sh

# sudo apt upgrade
# sudo apt update

# TODO
# - default fonts - not sure Source Code Pro is configured, need to increase default size
# - Bash prompt - Ideas - Current time - Location of logged in IP as flag emoji - Process exit code (powerline) - Powerline arrows - Mushroom/magic for not/root - username & machine
# - Emacs config
#   - Want / need to get typescript-ts-mode working (currently blocked on not having aarch64 treesitter ... or?). Ideal ts modes for: rust, swift.
#   - Confirm sbcl & clojure work out of the box [x] sbcl [x] bb
#   - General housecleaning
#   - migrate to modern use-package?
#
# - Sway
#   - Love the launcher & tiling modes :)
#   - Bug with cursor placement needs new wlroots https://gitlab.freedesktop.org/wlroots/wlroots/-/merge_requests/4621
#   - Color scheme to match shell
#   - Swaybar bells and whistles


