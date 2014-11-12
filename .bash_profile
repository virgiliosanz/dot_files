#!/usr/bin/env bash

PATH=~/Bin:/usr/local/bin:$PATH
export BASH_IT=$HOME/.bash_it

export BASH_IT_THEME='powerline-plain'
export EDITOR=vim
export GIT_EDITOR=vim
export SCM_CHECK=true

export GIT_HOSTING='git@gihub.com'

# Set vcprompt executable path for scm advance info in prompt (demula theme)
# https://github.com/xvzf/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt

# Load Bash It
source $BASH_IT/bash_it.sh
