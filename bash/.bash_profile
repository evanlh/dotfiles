if [ -f `brew --prefix`/etc/bash_completion ]; then
    . `brew --prefix`/etc/bash_completion
fi
function git_branch_for_cwd() { 2>&0; b=$(git symbolic-ref HEAD 2>/dev/null); [ "$b" ] && echo "[${b##*/}]"; }

PS1="\w \$(git_branch_for_cwd)\$ "

alias ls='ls -F'
alias vi='mvim -v'
alias gitlog="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
EDITOR='/usr/bin/mvim -v'

PATH=~/bin:/usr/local/bin:/usr/local/Cellar/php/5.3.10/bin/:/usr/local/sbin:$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

NODE_PATH=/usr/local/lib/node_modules

brew list --versions > ~/Dropbox/$HOSTNAME-brew.txt

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*
