sudo umount /proc/{cpuinfo,diskstats,meminfo,stat,uptime}
sh <(curl -L https://nixos.org/nix/install) --no-daemon
git clone git@github.com:evanlh/dotfiles.git
ln -s ~/dotfiles/dotemacs.el ~/.emacs.el
ssh-keygen
