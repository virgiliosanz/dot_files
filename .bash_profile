#!/usr/bin/env bash

PATH=/usr/local/sbin:$PATH
PATH=/usr/local/bin:$PATH
PATH=~/Bin:$PATH
export PATH

export BASH_IT=$HOME/.bash_it
export BASH_IT_THEME='powerline-plain'

export EDITOR=vim
export SCM_CHECK=true
export GIT_EDITOR=vim
export GIT_HOSTING='git@gihub.com'

# Set vcprompt executable path for scm advance info in prompt (demula theme)
# https://github.com/xvzf/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt
alias vim=/Applications/MacVim.app/Contents/MacOS/Vim
alias vi=/Applications/MacVim.app/Contents/MacOS/Vim

# Load Bash It
source $BASH_IT/bash_it.sh
