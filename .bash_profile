#!/usr/bin/env bash
#set -v
PATH=/usr/local/sbin:$PATH
PATH=/usr/local/bin:$PATH
PATH=/Users/vsanz/node_modules/.bin:$PATH
PATH=~/Bin:$PATH

export BASH_IT=$HOME/.bash_it
#export BASH_IT_THEME='powerline-multiline'
export BASH_IT_THEME='powerline-plain'
#export BASH_IT_THEME='sexy'
alias emacs="emacsclient -c -n "
export EDITOR=vim
#alias vim=emacs
alias vi=vim
export GIT_EDITOR=$EDITOR
export GIT_HOSTING='git@github.com'
export SCM_CHECK=true

export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# Set vcprompt executable path for scm advance info in prompt (demula theme)
# https://github.com/xvzf/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt

#export ANDROID_HOME=~/Library/Android/sdk
#PATH=$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools

export GOPATH=$HOME/go
PATH=$GOPATH/bin:$PATH

# VirtualEnv
export WORKON_HOME=~/.envs
export VIRTUALENVWRAPPER_HOOK_DIR=$WORKON_HOME/hooks
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
#export VIRTUALENVWRAPPER_VIRTUALENV_ARGS='--no-site-packages'
source /usr/local/bin/virtualenvwrapper.sh

export PATH

# Load Bash It
source $BASH_IT/bash_it.sh
