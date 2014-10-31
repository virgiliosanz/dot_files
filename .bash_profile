#!/usr/bin/env bash

# Load RVM, if you are using it
[[ -s $HOME/.rvm/scripts/rvm ]] && source $HOME/.rvm/scripts/rvm

# Path to the bash it configuration
export BASH_IT=$HOME/.bash_it

# Check if bash_it exists. Take action if not
if [ ! -d "$BASH_IT" ]; then
    git clone https://github.com/revans/bash-it.git ~/.bash_it
fi

export PATH=/opt/local/bin:/usr/local/bin:~/Bin:$PATH

# Lock and Load a custom theme file
# location /.bash_it/themes/
export BASH_IT_THEME='sexy'

# VIM
export EDITOR=vim
export PATH=/Applications/MacVim.app/Contents/MacOS/:$PATH
alias vi=vim


# https://github.com/xvzf/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt

# Android SDK
export ANDROID_HOME=~/Bin/adt-bundle-mac-x86_64-20140321/sdk
export PATH=$PATH:$ANDROID_HOME/tools:$ANDROID_HOME/platform-tools

# Maven
export MAVEN_HOME=~/Bin/apache-maven-3.2.1
export PATH=$PATH:$MAVEN_HOME/bin

# ant
export ANT_HOME=~/Bin/apache-ant-1.9.3
export PATH=$PATH:$ANT_HOME/bin

# Go
export GOPATH=$HOME/Code/gopath
export PATH=$PATH:$GOPATH/bin

# Emacs
#alias e=emacs
#alias vi=emacs
#alias vim=emacs


# Load Bash It
source $BASH_IT/bash_it.sh
