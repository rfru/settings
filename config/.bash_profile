alias e='emacsclient -n'
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin:$HOME/scripts

# Setting PATH for Python 2.7
# The orginal version is saved in .bash_profile.pysave
PATH="/usr/local/bin:${PATH}:/Users/mtlin/Library/Haskell/bin:/Users/mtlin/bin"
export PATH
alias ls='ls -G'

#export PS1='\[\e]0;\u@\h: \w\a\]${debian_chroot:+($debian_chroot)}\u@\h:\w$(__git_ps1 "(%s)")\$ '
reset=$(tput sgr0)
bold=$(tput bold)
black=$(tput setaf 0)
red=$(tput setaf 1)
green=$(tput setaf 2)
yellow=$(tput setaf 3)
blue=$(tput setaf 4)
magenta=$(tput setaf 5)
cyan=$(tput setaf 6)
white=$(tput setaf 7)
if [ -f /usr/lib/git-core/git-sh-prompt ]; then
  source /usr/lib/git-core/git-sh-prompt
  export PS1='\h:\w\[$bold\]$(__git_ps1 " %s")\[$reset\] $ '
elif [ -f /usr/local/etc/bash_completion.d/git-prompt.sh ]; then
  source /usr/local/etc/bash_completion.d/git-prompt.sh
  export PS1='\h:\w\[$bold\]$(__git_ps1 " %s")\[$reset\] $ '
else
  export PS1='\h:\w $ '
fi
