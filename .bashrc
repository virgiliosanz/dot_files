# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ "$TERM" != 'dumb' ] && [ -n "$BASH" ] && [ -n "$PS1" ]
then
   if [ `/usr/bin/whoami` = 'root' ]
   then
      export PS1='\[\033[01;31m\]\u@\h \[\033[01;34m\]\w\n\$ \[\033[00m\]'
   else
      export PS1='\[\033[01;32m\]\u@\h \[\033[01;34m\]\w\n\$ \[\033[00m\]'
   fi
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

export JAVA_HOME="/System/Library/Frameworks/JavaVM.framework/Home/"
PATH="$PATH:$JAVA_HOME/bin"
# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi
PATH="/usr/local/bin":$PATH
PATH="/usr/local/sbin":$PATH
export PATH
export EDITOR=vim

# AWS
alias cocinario_dev='ssh -i ~/Dropbox/\@Cocinario/AWS/cocinario.pem ubuntu@ec2-79-125-52-198.eu-west-1.compute.amazonaws.com'
alias cocinario_front='ssh -i ~/Dropbox/\@Cocinario/AWS/cocinario.pem ec2-user@ec2-46-137-49-2.eu-west-1.compute.amazonaws.com'

alias v_dev='ssh -i ~/Dropbox/Personal/virgiliosanz.me/virgiliosanzme.pem ubuntu@ec2-79-125-76-54.eu-west-1.compute.amazonaws.com'

