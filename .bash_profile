#!/usr/bin/env bash

PATH=~/Bin:/usr/local/bin:/opt/boost/bin:$PATH

# Path to the bash it configuration
export BASH_IT=$HOME/.bash_it

# Lock and Load a custom theme file
# location /.bash_it/themes/
export BASH_IT_THEME='sexy'

# VIM
export EDITOR=vim
PATH=/Applications/MacVim.app/Contents/MacOS:$PATH
alias vi=vim


export PATH

# Load Bash It
source $BASH_IT/bash_it.sh

