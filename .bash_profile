#!/bin/bash -x

#set -v
#set -x
PATH=/usr/local/sbin:$PATH
PATH=/usr/local/bin:$PATH

PATH="$HOME/bin:$PATH"
PATH="$HOME/.local/bin:$PATH"
PATH="$HOME/bin/go/bin:$PATH"
PATH="$HOME/bin/zig:$PATH"
PATH="$HOME/cmake/zig:$PATH"
export PATH

# aliases to hosts
. "$HOME/.aliases"

# Editors
export EDITOR=vim

# Languages
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# GIT
export GIT_EDITOR=$EDITOR
export GIT_HOSTING='git@github.com'
export SCM_CHECK=true

# Bash-It
export BASH_IT="${HOME}/.bash_it"
# export BASH_IT_THEME='powerline'
# export BASH_IT_THEME='powerline-naked'
# export BASH_IT_THEME='powerline-plain'
# export BASH_IT_THEME='powerline-multiline'
# export SEXY_THEME_SHOW_PYTHON=true
export BASH_IT_THEME='sexy'
# export BASH_IT_THEME='rjorgenson'
# export BASH_IT_THEME='bakke'

# C/C++

# Add ssh key

# SSH Agent should be running, once
# eval "$(ssh-agent -s)"

# >>> juliaup initialize >>>

# !! Contents within this block are managed by juliaup !!

case ":$PATH:" in
*:/home/v/.juliaup/bin:*) ;;

*)
  export PATH=/home/v/.juliaup/bin${PATH:+:${PATH}}
  ;;
esac

# <<< juliaup initialize <<<

# RUST
. "$HOME/.cargo/env"

# Golang
export GOPATH=$HOME/bin/go
PATH=$GOPATH/bin:$PATH
export PATH

# k0s configuration
export KUBECONFIG=~/Code/k8s/k0sctl/kubeconfig

# Fuzzi
. "$HOME/.fzf.bash"

# ollama
# export OLLAMA_HOST=0.0.0.0:11435

. "$HOME/.bash_it/bash_it.sh"
