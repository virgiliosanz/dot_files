#!/usr/bin/env bash

PATH=~/Bin:/usr/local/bin:$PATH

# Path to the bash it configuration
export BASH_IT=$HOME/.bash_it

# Lock and Load a custom theme file
# location /.bash_it/themes/
#export BASH_IT_THEME='sexy'
#export BASH_IT_THEME='hawaii50'
#export BASH_IT_THEME='doubletime'
#export BASH_IT_THEME='doubletime_multiline'
#export BASH_IT_THEME='doubletime_multiline_pyonly'
#export BASH_IT_THEME='iterate'
export BASH_IT_THEME='powerline-plain'
#export BASH_IT_THEME='powerline'
#export BASH_IT_THEME='tylenol'

# VIM
export EDITOR=vim
PATH=/Applications/MacVim.app/Contents/MacOS:$PATH
alias vi=vim


export PATH

# Load Bash It
source $BASH_IT/bash_it.sh

