# If not running interactively, don't do anything
[ -z "$PS1" ] && return

function source_if_exists() {
    if [ -d $1 ]; then
        for f in $1/*; do
            . $f
        done
    
    elif [ -f $1 ]; then
        . $1
    fi
}

########## Configuración 
source_if_exists /etc/profile
source_if_exists /etc/bash.bashrc
source_if_exists ~/.aliases
source_if_exists ~/.bash_aliases
source_if_exists ~/.bash_completion
source_if_exists ~/.bash_prompt

########### VIM
export EDITOR=vim

########### CASTELLANO
export LANGUAGE="es_ES"
export LC_MESSAGES="es_ES.UTF-8"
export LC_CTYPE="es_ES.UTF-8"
export LC_COLLATE="es_ES.UTF-8"

########### JAVA
export JAVA_HOME="/System/Library/Frameworks/JavaVM.framework/Home/"
PATH=$JAVA_HOME/bin:$PATH

########### Haskell
PATH=$HOME/Library/Haskell/bin:$PATH

########### Go Lang
export GOPATH=~/Code/gopath
export GOROOT=/usr/local/go/
PATH=$GOPATH/bin:$GOROOT/bin:$PATH

########### PATH
PATH=/usr/local/sbin:$PATH
PATH=/usr/local/bin:$PATH
PATH=/opt/local/bin/:$PATH
PATH=$HOME/Bin:$PATH
export PATH

