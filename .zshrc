# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="xiong-chiamiov-plus"
ZSH_THEME="steeef"
#ZSH_THEME='ys'
#ZSH_THEME='juanghurtado'
#ZSH_THEME='af-magic'
#ZSH_THEME='agnoster'
#ZSH_THEME='dstufft'
#ZSH_THEME='fino-time' # Modificar este y convertirlo en el nuev vsanz
#ZSH_THEME='vsanz'

# Example aliases
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
plugins=(git git-flow osx vagrant macports python ssh-agent virtualenv)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=$HOME/ node_modules/.bin/:$HOME/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/usr/local/sbin:/bin:/sbin:/usr/bin:/usr/sbin
alias lsg="ssh -2 -i ~/.ssh/deployed/2014-01-08 vsanz@lsg-west.akamai.com"
alias myvm="ssh -2 -i ~/.ssh/internal/2013-09-09  vsanz@mad-lvovi.munich.corp.akamai.com"

# Go Lang
export GOPATH=~/Code/gopath
export GOROOT=/opt/local/go/
PATH=$GOPATH/bin:$GOROOT/bin:$PATH

export LSCOLORS=gxfxbEaEBxxEhEhBaDaCaD

# Configuración de algunos plugins de oh-my-zsh
zstyle :omz:plugins:ssh-agent agent-forwarding on
zstyle :omz:plugins:ssh-agent identities id_rsa deployed/2014-01-08 internal/2014-01-08 internal/2013-09-09
