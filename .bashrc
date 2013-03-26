# Ryan's .bashrc

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Load any local settings
[[ -s "$HOME/.bashrc.local" ]] && source "$HOME/.bashrc.local"

HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
HISTCONTROL=ignoreboth

export HISTSIZE=1500
export HISTFILESIZE=2000
shopt -s histappend
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

if [ "$COLORTERM" == "gnome-terminal" ] && [ -z "$TMUX" ]; then
    export TERM=xterm-256color
fi

# Utility Functions for Prompt:
source "$HOME/.bash_palette"

display_project_env() {
    # Displays virtualenv information and VCS branch.
    local open_parens="${WHITE}("
    local close_parens="${WHITE})"
    local sep="${WHITE}//"

    local git_branch=$(__git_ps1 | sed -e 's/[ ()]//g')
    local git_branch_display="${BOLD_GREEN}${git_branch}${close_parens}"
    local virtualenv=$([[ -z "$VIRTUAL_ENV" ]] && echo '' || echo $(basename $VIRTUAL_ENV))
    local virtualenv_display="${open_parens}${BOLD_CYAN}${virtualenv}"

    if [[ "$virtualenv" && "$git_branch" ]]; then
        project_env="${virtualenv_display}${sep}${git_branch_display}"
    elif [[ "$virtualenv" && -z "$git_branch" ]]; then
        project_env="${virtualenv_display}${close_parens}"
    elif [[ -z "$virtualenv" && "$git_branch" ]]; then
        project_env="${open_parens}${git_branch_display}";
    else
        project_inv=
    fi

    echo $project_env
}

jobs_count() {
    # Formats and returns the jobs count, in the format 'jobs: NUM'.
    if [[ $(jobs | wc -l | tr -d " ") -gt 0 ]]; then
        echo " ${YELLOW}[${PURPLE}jobs: \j${YELLOW}]";
    fi
}

main_prompt() {
    # Takes the cached exit status as its only argument; returns an
    # appropriately coloured prompt, using the appropriate symbol ($ for
    # standard user, # for root user).
    if [[ $1 = 0 ]]; then
        echo "${BOLD_GREEN}\$"
    else
        echo "${BOLD_RED}\$"
    fi
}

hist_num="${YELLOW}[${BOLD_GREEN}\!${YELLOW}]"
user_sys_info="${BOLD_BLUE}\u${YELLOW}@${BOLD_BLUE}\h"
time_stamp="${YELLOW}[${RED}\t${YELLOW}]"
cwd_path="${YELLOW}[ ${BOLD_BLUE}\w${YELLOW} ]"
PROMPT_COMMAND='__exit_status=$?;
PS1="\n${hist_num} ${user_sys_info} ${time_stamp} $(display_project_env)
${cwd_path} $(jobs_count)
$(main_prompt $__exit_status)${RESET} "'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# Personl settings
export PAGER=/usr/bin/less
export EDITOR=/usr/bin/emacsclient

# Personal Aliases
alias ls="ls -hF --color=auto"
alias la="ls -Al"
alias ll="ls -l"
alias lr="ls -R"
alias lx="ll -BX" # sort by extension
alias lz="ll -rS" # sort by size
alias lt="ll -rt" # sort by date
alias tree="tree -Csuh"
alias duh="du -h --max-depth=1"
alias rm="rm -I --preserve-root"
alias meminfo="free -mlt"
alias top="htop"
alias woll='workon ${LL_VIRTUALENV:-"haystack"}'

function psgrep() { ps axuf | grep -v grep | grep "$@" -i --color=auto; }
function fname() { find . -iname "*$@*"; }

# Load virtualenvwrapper extensions
export VIRTUAL_ENV_DISABLE_PROMPT=1
if [ $USER == ryan ]; then
   export WORKON_HOME=$HOME/.virtualenvs
   source /usr/local/bin/virtualenvwrapper.sh
fi

_pip_completion()
{
    COMPREPLY=( $( COMP_WORDS="${COMP_WORDS[*]}" \
                   COMP_CWORD=$COMP_CWORD \
                   PIP_AUTO_COMPLETE=1 $1 ) )
}
complete -o default -F _pip_completion pip

export PATH=${HOME}/opt/go/bin:${HOME}/opt/texlive/2011/bin/x86_64-linux:${HOME}/.rbenv/bin:$PATH

# rbenv completion
eval "$(rbenv init -)"

export GOROOT=${HOME}/opt/go
export GOARCH=amd64
export GOOS=linux

remtrail() {
  if [[ -z $1 ]]
  then
      echo "You must supply an extension."
      return 1
  fi

  find . -name "*.$1" -type f -exec sed -i 's/ *$//' '{}' ';'
}

# [ -s "${HOME}/.scm_breeze/scm_breeze.sh" ] && source "${HOME}/.scm_breeze/scm_breeze.sh"
