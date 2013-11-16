alias e='emacsclient -n'
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin:$HOME/scripts

# Setting PATH for Python 2.7
# The orginal version is saved in .bash_profile.pysave
PATH="/usr/local/bin:${PATH}:/Users/mtlin/Library/Haskell/bin:/Users/mtlin/bin"
export PATH
alias ls='ls -G'

source ~/scripts/remote/.bash_aliases
