#!/usr/bin/env bash

# Añado path local
PATH=~/Bin:$PATH

# Path to the bash it configuration
export BASH_IT=$HOME/.bash_it

# Lock and Load a custom theme file
# location /.bash_it/themes/
export BASH_IT_THEME='sexy'

# VIM
export EDITOR=vim
alias vi=vim

# Go
#export GOPATH=$HOME/Code/gopath
#PATH=$PATH:$GOPATH/bin

export PATH

# Load Bash It
source $BASH_IT/bash_it.sh
