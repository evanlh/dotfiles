# FROM http://blog.tonyscelfo.com/2009/04/save-all-of-your-bash-history.html
# don't put duplicate lines in the history. See bash(1) for more options
# ... and ignore same sucessive entries.
export HISTCONTROL=ignoreboth

# set the time format for the history file.
export HISTTIMEFORMAT="%Y.%m.%d %H:%M:%S "

# If this is an xterm set the title to user@host:dir
case "$TERM" in
  xterm*|rxvt*)
  # Show the currently running command in the terminal title:
  # http://www.davidpashley.com/articles/xterm-titles-with-bash.html
  show_command_in_title_bar()
  {
    case "$BASH_COMMAND" in
      *\033]0*)
      # The command is trying to set the title bar as well;
      # this is most likely the execution of $PROMPT_COMMAND.
      # In any case nested escapes confuse the terminal, so don't
      # output them.
      ;;
      *)
      if test ! "$BASH_COMMAND" = "log_bash_eternal_history"
      then
        echo -ne "\033]0;$(history 1 | sed 's/^ *[0-9]* *//') :: ${PWD} :: ${USER}@${HOSTNAME}\007"
      fi
      ;;
    esac
  }
  trap show_command_in_title_bar DEBUG
  ;;
  *)
  ;;
esac

log_bash_eternal_history()
{
  local rc=$?
  [[ $(history 1) =~ ^\ *[0-9]+\ +([^\ ]+\ [^\ ]+)\ +(.*)$ ]]
  local date_part="${BASH_REMATCH[1]}"
  local command_part="${BASH_REMATCH[2]}"
  if [ "$command_part" != "$ETERNAL_HISTORY_LAST" -a "$command_part" != "ls" -a "$command_part" != "ll" ]
  then
    echo $date_part $HOSTNAME $rc "$command_part" >> ~/.bash_eternal_history
    export ETERNAL_HISTORY_LAST="$command_part"
  fi
}

PROMPT_COMMAND="log_bash_eternal_history"
source ~/.fredsh

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

eval "$(/usr/libexec/path_helper)"

