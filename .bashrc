# Ryan's .bashrc

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Load any local settings
[[ -s "$HOME/.bashrc.local" ]] && source "$HOME/.bashrc.local"

# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoreboth

# Remember more than the default 500 items
export HISTSIZE=1500
export HISTFILESIZE=1500

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

if [ "$COLORTERM" == "gnome-terminal" ] && [ -z "$TMUX" ]; then
    export TERM=xterm-256color
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

# Color Palette:
# See <url:http://www.ibm.com/developerworks/linux/library/l-tip-prompt/>
# Color chart for full selection
export black=$'\e[00;30m'
export dark_grey=$'\e[01;30m'
export red=$'\e[00;31m'
export bold_red=$'\e[01;31m'
export green=$'\e[00;32m'
export bold_green=$'\e[01;32m'
export brown=$'\e[00;33m'
export yellow=$'\e[01;33m'
export blue=$'\e[00;34m'
export bold_blue=$'\e[01;34m'
export purple=$'\e[00;35m'
export bold_purple=$'\e[01;35m'
export cyan=$'\e[00;36m'
export bold_cyan=$'\e[01;36m'
export bold_grey=$'\e[00;37m'
export white=$'\e[01;37m'
export reset=$'\e[0m'

# We need to surround all non-printing characters with special bash escape
# sequences, "\[" and "\]". These sequences will tell bash that the enclosed
# characters don't take up any space on the line, which will allow
# word-wrapping to continue to work properly. Without them, you'll end up with
# a nice-looking prompt that will mess up the screen if you happen to type in a
# command that approaches the extreme right of the terminal.
# Source: <url:http://www.ibm.com/developerworks/linux/library/l-tip-prompt/>
BLACK='\[${black}\]'
DARK_GREY='\[${dark_grey}\]'
RED='\[${red}\]'
BOLD_RED='\[${bold_red}\]'
GREEN='\[${green}\]'
BOLD_GREEN='\[${bold_green}\]'
BROWN='\[${brown}\]'
YELLOW='\[${yellow}\]'
BLUE='\[${blue}\]'
BOLD_BLUE='\[${bold_blue}\]'
PURPLE='\[${purple}\]'
BOLD_PURPLE='\[${bold_purple}\]'
CYAN='\[${cyan}\]'
BOLD_CYAN='\[${bold_cyan}\]'
BOLD_GREY='\[${bold_grey}\]'
WHITE='\[${white}\]'
RESET='\[${reset}\]'

# Utility Functions for Prompt:

GIT_PROMPT="$HOME/bin/git-prompt.sh"
if [[ -f "$GIT_PROMPT" ]]
then
    source $GIT_PROMPT
    git_prompt=true
fi

display_project_env() {
    # Displays virtualenv information and VCS branch.
    if [[ -n "$git_prompt" ]]
    then
        local git_branch=$(__git_ps1 | sed -e 's/[ ()]//g')
    else
        local git_branch="missing git-prompt.sh"
    fi

    local virtualenv=$([[ -z "$VIRTUAL_ENV" ]] && echo '' || echo $(basename $VIRTUAL_ENV))
    local gemset= # $(rvm-prompt g | sed s/@//)

    # Flag to determine whether or not to add a closing parenthesis
    local close_env=true

    if [[ -n "$virtualenv" ]] && [[ -z "$gemset" ]]; then
        project_env="${WHITE}(${BOLD_CYAN}${virtualenv}${WHITE}"
    elif [[ -n "$gemset" ]] && [[ -z "$virtualenv" ]]; then
        project_env="${WHITE}(${BOLD_RED}${gemset}${WHITE}"
    elif [[ -n "$gemset" ]] && [[ -n "$virtualenv" ]]; then
        project_env="${WHITE}(${BOLD_CYAN}${virtualenv}${WHITE}|${BOLD_RED}${gemset}${WHITE}"
    else
        close_env=false
        project_env=""
    fi

    local git_branch_display="${BOLD_GREEN}${git_branch}${WHITE})"

    if [[ -n "$git_branch" ]] && $close_env; then
        project_env="${project_env}${WHITE}//${git_branch_display}"
    elif [[ -n "$git_branch" ]] && ! $close_env; then
        project_env="${WHITE}(${git_branch_display}"
    elif [[ -z "$git_branch" ]] && $close_env; then
        project_env="${project_env})"
    fi

    echo $project_env
}

jobs_count() {
    # Formats and returns the jobs count, in the format 'jobs: NUM'.
    if [[ $(jobs | wc -l | tr -d " ") -gt 0 ]]; then
        echo " ${YELLOW}[${PURPLE}jobs: \j${YELLOW}]";
    fi
}
cache_exit_status() {
    # Stores the exit status of the previously executed command: /this/ is the
    # value we're interested in if we want to indicate the previous command's
    # success/failure status via our prompt.
    # If we don't use this cached value, it will instead use the
    # exit status of the last run utility function, and will therefore always
    # display as a success.
    exit_status="$?"
    echo $exit_status
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

update_titlebar() {
    # Updates the titlebar for suitable terminals.
    case $TERM in
        xterm*|rxvt*|screen*)
            local TITLEBAR='\[\033]0;\u@\h:\w\007\]'
            echo $TITLEBAR
            ;;
        *)
            local TITLEBAR=""
            echo $TITLEBAR
            ;;
    esac
}

hist_num="${YELLOW}[${BOLD_GREEN}\!${YELLOW}]"
user_sys_info="${BOLD_BLUE}\u${YELLOW}@${BOLD_BLUE}\h"
time_stamp="${YELLOW}[${RED}\t${YELLOW}]"
cwd_path="${YELLOW}[ ${BOLD_BLUE}\w${YELLOW} ]"
PROMPT_COMMAND='status=$(cache_exit_status);
PS1="$(update_titlebar)\n${hist_num} ${user_sys_info} ${time_stamp} $(display_project_env)
${cwd_path} $(jobs_count)
$(main_prompt $status)${RESET} "'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    #alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# Personl settings
export PAGER=/usr/bin/less
export EDITOR=/usr/bin/emacsclient

# Personal Aliases
alias ls="ls --color=auto"
alias lsa="ls -Al --color=auto"
alias lsl="ls -l --color=auto"
alias duh="du -h --max-depth=1"
alias woll='workon ${LL_VIRTUALENV:-"haystack"}'

# Load virtualenvwrapper extensions
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
export JDK_HOME=${HOME}/opt/java/jdk1.7.0_05

[ -s "${HOME}/.scm_breeze/scm_breeze.sh" ] && source "${HOME}/.scm_breeze/scm_breeze.sh"
