# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="steeef"
#ZSH_THEME="trapd00r"
#ZSH_THEME="xiong-chiamiov-plus"
#ZSH_THEME="ys"
#ZSH_THEME="bira"
ZSH_THEME="fino-time"

source $HOME/.aliases

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how many often would you like to wait before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git git-flow macos vagrant macports python ssh-agent virtualenv)

source $ZSH/oh-my-zsh.sh
zstyle :omz:plugins:ssh-agent agent-forwarding on
zstyle :omz:plugins:ssh-agent identities id_rsa vsanz_proofpoint

# Customize to your needs...
PATH=/sbin:$PATH
PATH=/usr/sbin:$PATH
PATH=/bin:$PATH
PATH=/usr/bin:$PATH
PATH=/opt/local/sbin:$PATH
PATH=/opt/local/bin:$PATH
PATH=/usr/local/sbin:$PATH
PATH=/usr/local/bin:$PATH
PATH=$HOME/Bin:$PATH
PATH=$HOME/node_modules/.bin/:$PATH
PATH=$HOME/.cargo/bin:$PATH
PATH=/opt/metasploit-framework/bin:$PATH
export PATH

export LSCOLORS=gxfxbEaEBxxEhEhBaDaCaD
export LANG=en_US.UTF-8

# Editors
export EDITOR=vim
alias vi=$EDITOR

# Languages
export LANGUAGE=en_US.UTF-8
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# GIT
export GIT_EDITOR=$EDITOR
export GIT_HOSTING='git@github.com'
export SCM_CHECK=true

# VirtualEnv
# Setting PATH for Python 3 installed by brew
export PATH=/usr/local/share/python:$PATH

# Configuration for virtualenv
export WORKON_HOME=$HOME/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python3
export VIRTUALENVWRAPPER_VIRTUALENV=/usr/local/bin/virtualenv
source /usr/local/bin/virtualenvwrapper.sh
