#!/usr/bin/env bash

PATH=/usr/local/sbin:$PATH
PATH=/usr/local/bin:$PATH
PATH=/opt/local/sbin:$PATH
PATH=/opt/local/bin:$PATH
#PATH=/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin/:$PATH
PATH=/Users/vsanz/node_modules/.bin:$PATH
PATH=~/Bin:$PATH
export PATH

export BASH_IT=$HOME/.bash_it
export BASH_IT_THEME='powerline-plain'
#export BASH_IT_THEME='sexy'
alias vim='mvim -v '
alias vi=vim
export EDITOR=vi
export EDITOR=vi
export GIT_EDITOR=$EDITOR
export GIT_HOSTING='git@gihub.com'
export SCM_CHECK=true

# Set vcprompt executable path for scm advance info in prompt (demula theme)
# https://github.com/xvzf/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt

export GOPATH=$HOME/go
PATH=$GOPATH/bin:$PATH

export PATH
# Load Bash It
source $BASH_IT/bash_it.sh
