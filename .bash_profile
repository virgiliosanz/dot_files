#!/usr/bin/env bash
#set -v
PATH=/usr/local/sbin:$PATH
PATH=/usr/local/bin:$PATH
PATH=/opt/local/sbin:$PATH
PATH=/opt/local/bin:$PATH
PATH=/opt/local/Library/Frameworks/Python.framework/Versions/2.7/bin/:$PATH
PATH=/opt/local/Library/Frameworks/Python.framework/Versions/3.6/bin:$PATH
PATH=/Users/vsanz/node_modules/.bin:$PATH
PATH=~/Bin:$PATH
export PATH

export BASH_IT=$HOME/.bash_it
export BASH_IT_THEME='powerline-plain'
#export BASH_IT_THEME='sexy'
export EDITOR='mvim -v '
alias vim=$EDITOR
alias vi=vim
alias emacs="/Applications/MacPorts/Emacs.app/Contents/MacOS/bin/emacsclient -nw -c "
#alias vim="/Applications/MacPorts/Emacs.app/Contents/MacOS/bin/emacsclient -nw -t "
export GIT_EDITOR=$EDITOR
export GIT_HOSTING='git@gihub.com'
export SCM_CHECK=true

# Set vcprompt executable path for scm advance info in prompt (demula theme)
# https://github.com/xvzf/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt

export ANDROID_HOME=~/Library/Android/sdk
export PATH=${PATH}:${ANDROID_HOME}/tools
export PATH=${PATH}:${ANDROID_HOME}/platform-tools

export GOPATH=$HOME/go
PATH=$GOPATH/bin:$PATH

# iTerm2 shell integration
test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

export PATH
# Load Bash It
source $BASH_IT/bash_it.sh

