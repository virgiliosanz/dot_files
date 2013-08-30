alias ls='ls -G'
#alias ls='ls --color=auto'
#alias dir='dir --color=auto'
#alias vdir='vdir --color=auto'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

alias df='df -h'

alias vi=vim
#alias vim='mvim '

alias mkdir='mkdir -pv'

# do not delete / or prompt if deleting more than 3 files at a time #
# alias rm='rm --preserve-root' # no funciona en OSX
 
# confirmation
alias mv='mv -i'
alias cp='cp -i'
alias ln='ln -i'
  
# Parenting changing perms on / 
# No funiona en OSX
#alias chown='chown --preserve-root'
#alias chmod='chmod --preserve-root'
#alias chgrp='chgrp --preserve-root'

alias ports='netstat -tulanp'
alias fastping='ping -c 100 -s 2'

# get web server headers #
alias header='curl -I'
 
# find out if remote server supports gzip / mod_deflate or not #
 alias headerc='curl -I --compress'
 
# Resume by default
alias wget='wget -c'
