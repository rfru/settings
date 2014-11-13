export LANG='en_US.UTF-8'
export LC_ALL="en_US.UTF-8"
export GOPATH=$HOME/go
export EDITOR=e
export PATH=$GOPATH/bin:$HOME/settings:$HOME/bin:$HOME/Library/Haskell/bin:/Applications/Emacs.app/Contents/MacOS/bin:/usr/local/bin:$PATH

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    alias ls='ls --color=auto'
elif [[ "$unamestr" == 'Darwin' ]]; then
    alias ls='ls -G'
fi

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

export PS1='\h:\w $ '

if [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi
