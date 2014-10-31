#!/usr/bin/env bash

# Load RVM, if you are using it
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm

PATH=/opt/local/bin:/usr/local/bin:~/Bin:$PATH

# Path to the bash it configuration
export BASH_IT=$HOME/.bash_it

# Lock and Load a custom theme file
# location /.bash_it/themes/
export BASH_IT_THEME='sexy'

# VIM
export EDITOR=vim
PATH=/Applications/MacVim.app/Contents/MacOS/:$PATH
alias vi=vim

# Android SDK
export ANDROID_HOME=~/Bin/adt-bundle-mac-x86_64-20140321/sdk
PATH=$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools

# Maven
export MAVEN_HOME=~/Bin/apache-maven-3.2.1
PATH=$PATH:$MAVEN_HOME/bin

# ant
export ANT_HOME=~/Bin/apache-ant-1.9.3
PATH=$PATH:$ANT_HOME/bin

# Go
export GOPATH=$HOME/Code/gopath
PATH=$PATH:$GOPATH/bin

export PATH

# Load Bash It
source $BASH_IT/bash_it.sh
