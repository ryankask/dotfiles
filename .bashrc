# Ryan's .bashrc

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

HISTCONTROL="ignoreboth"
HISTSIZE=10000
HISTFILESIZE=1000000
HISTIGNORE="&:ls:cd ~:cd ..:[bf]g:exit:history"
HISTTIMEFORMAT="%F %T "
shopt -s histappend
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[[ -x "/usr/bin/lesspipe" ]] && eval "$(SHELL=/bin/sh lesspipe)"

if [[ -n "$TERM_PROGRAM" ]] && [[ -z "$TMUX" ]] || [[ $COLORTERM == "gnome-terminal" ]]; then
    export TERM="xterm-256color"
fi

# Utility Functions for Prompt:
source "$HOME/.bash_palette"

display_project_env() {
    # Displays virtualenv information and VCS branch.
    local open_parens="${WHITE}("
    local close_parens="${WHITE})"
    local sep="${WHITE}//"

    local git_branch="$(__git_ps1 | sed -e 's/[ ()]//g')"
    local git_branch_display="${BOLD_GREEN}${git_branch}${close_parens}"
    local virtualenv="$([[ -z "$VIRTUAL_ENV" ]] && echo '' || echo $(basename $VIRTUAL_ENV))"
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

prompt_command() {
    __exit_status=$?
    local hist_num user_sys_info time_stamp cwd_path main_prompt jobs_count project_info

    hist_num="${BOLD_YELLOW}[${BOLD_GREEN}\!${BOLD_YELLOW}]"
    user_sys_info="${BOLD_BLUE}\u${BOLD_YELLOW}@${BOLD_BLUE}\h"
    time_stamp="${BOLD_YELLOW}[${RED}\t${BOLD_YELLOW}]"
    project_info="$(type -t __git_ps1 > /dev/null 2>&1 && display_project_env)"
    cwd_path="${BOLD_YELLOW}[ ${BOLD_BLUE}\w${BOLD_YELLOW} ]"

    if [[ $(jobs | wc -l | tr -d " ") -gt 0 ]]; then
        jobs_count=" ${BOLD_YELLOW}[${PURPLE}jobs: \j${BOLD_YELLOW}]";
    fi

    if [[ $__exit_status == 0 ]]; then
        main_prompt="${BOLD_GREEN}\$"
    else
        main_prompt="${BOLD_RED}\$"
    fi

    PS1="\n${hist_num} ${user_sys_info} ${time_stamp} ${project_info}
${cwd_path} ${jobs_count}
${main_prompt}${RESET} "

    history -a
}

PROMPT_COMMAND=prompt_command

# OS specific settings

case "$OSTYPE" in
    darwin*)
        BREW_PREFIX="$(brew --prefix)"

        if ! shopt -oq posix && [[ -f "$BREW_PREFIX/etc/bash_completion" ]]; then
            . $BREW_PREFIX/etc/bash_completion
        fi

        export PATH="$BREW_PREFIX/opt/coreutils/libexec/gnubin:$HOME/.cabal/bin:/usr/local/bin:$PATH"
        export MANPATH="$BREW_PREFIX/opt/coreutils/libexec/gnuman:$MANPATH"
        export EDITOR="emacs"

        source $BREW_PREFIX/opt/chruby/share/chruby/chruby.sh
        chruby 2.1

        # OS X-specific aliases
        alias find="$BREW_PREFIX/bin/gfind"
        alias xargs="$BREW_PREFIX/bin/gxargs"

        PG_DIR="$BREW_PREFIX/var/postgres"
        alias pg_start="pg_ctl -D $PG_DIR -l $PG_DIR/server.log start"
        alias pg_stop="pg_ctl -D $PG_DIR stop -s -m fast"

        [[ -s "$BREW_PREFIX/etc/autojump.sh" ]] && . $BREW_PREFIX/etc/autojump.sh
        ;;
    linux-gnu)
        # enable color support of ls and also add handy aliases

        if ! shopt -oq posix; then
            if [[ -f "/usr/share/bash-completion/bash_completion" ]]; then
                . /usr/share/bash-completion/bash_completion
            elif [[ -f "/etc/bash_completion" ]]; then
                . /etc/bash_completion
            fi
        fi

        if [[ -x "/usr/bin/dircolors" ]]; then
            test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
            alias grep="grep --color=auto"
            alias fgrep="fgrep --color=auto"
            alias egrep="egrep --color=auto"
        fi

        export PAGER="/usr/bin/less"
        export EDITOR="/usr/bin/emacsclient -a gedit"
        export GOROOT="$HOME/opt/go"
        export PATH="$GOROOT/bin:$HOME/.rbenv/bin:$PATH"

        # Linux-specific aliases
        alias meminfo="free -mlt"

        # Linux-specific commands
        psgrep() { ps axuf | grep -v grep | grep "$@" -i --color=auto; }
        ;;
esac

# Personal Aliases
alias ls="ls -hF --color=auto"
alias la="ls -Al"
alias lh="ls -Ahl"
alias ll="ls -l"
alias lr="ls -R"
alias lx="ll -BX" # sort by extension
alias lz="ll -rS" # sort by size
alias lt="ll -rt" # sort by date
alias tree="tree -Csuh"
alias duh="du -h --max-depth=1"
alias rm="rm -I --preserve-root"
alias top="htop"
alias cleanpyc="find . -name \"*.pyc\" -delete"
alias gcm="git checkout master"

fname() { find . -iname "*$@*"; }
remtrail() {
  if [[ -z "$1" ]]; then
      echo "You must supply an extension."
      return 1
  fi

  find . -name "*.$1" -type f -exec sed -i 's/ *$//' '{}' ';'
}

# Load virtualenvwrapper extensions
if [[ $(id -u ) != 0 ]]; then
    export VIRTUAL_ENV_DISABLE_PROMPT=1
    export WORKON_HOME=$HOME/.virtualenvs
    export PIP_VIRTUALENV_BASE=$WORKON_HOME
    export PIP_REQUIRE_VIRTUALENV=true
    export PIP_RESPECT_VIRTUALENV=true
    [[ -e "/usr/local/bin/virtualenvwrapper.sh" ]] && source "/usr/local/bin/virtualenvwrapper.sh"
fi

_pip_completion() {
    COMPREPLY=( $( COMP_WORDS="${COMP_WORDS[*]}" \
                   COMP_CWORD=$COMP_CWORD \
                   PIP_AUTO_COMPLETE=1 $1 ) )
}
complete -o default -F _pip_completion pip

[[ -s "$HOME/.nvm/nvm.sh" ]] && . $HOME/.nvm/nvm.sh
[[ -r "$NVM_DIR/bash_completion" ]] && . $NVM_DIR/bash_completion

# Load any local settings
[[ -s "$HOME/.bashrc.local" ]] && . "$HOME/.bashrc.local"
