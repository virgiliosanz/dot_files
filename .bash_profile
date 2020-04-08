#!/usr/bin/env bash

#set -v
PATH=/usr/local/sbin:$PATH
PATH=/usr/local/bin:$PATH
PATH=~/Bin:$PATH

# Set vcprompt executable path for scm advance info in prompt (demula theme)
# https://github.com/xvzf/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt

#export ANDROID_HOME=~/Library/Android/sdk
#PATH=$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools

# aliases to hosts
. $HOME/.aliases

# Golang
#export GOPATH=$HOME/go
#PATH=$GOPATH/bin:$PATH
#export PATH

# Rust
PATH="$HOME/.cargo/bin:$PATH"

# Editors
export EDITOR=vim
alias vi=$EDITOR

# Languages
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# GIT
export GIT_EDITOR=$EDITOR
export GIT_HOSTING='git@github.com'
export SCM_CHECK=true

# VirtualEnv
export WORKON_HOME=~/.envs
export PIP_VIRTUALENV_BASE=$WORKON_HOME
export PIP_RESPECT_VIRTUALENV=true
export VIRTUALENVWRAPPER_HOOK_DIR=$WORKON_HOME/hooks
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
#export VIRTUALENVWRAPPER_VIRTUALENV_ARGS='--no-site-packages'
. /usr/local/bin/virtualenvwrapper.sh


export PATH

# Bash-It
export BASH_IT=$HOME/.bash_it
#export BASH_IT_THEME='powerline-multiline'
#export BASH_IT_THEME='sexy'
export BASH_IT_THEME='powerline-plain'

source $BASH_IT/bash_it.sh

