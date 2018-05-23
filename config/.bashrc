export LANG='en_US.UTF-8'
export LC_ALL="en_US.UTF-8"
export GOPATH=$HOME/go
export PATH=/usr/local/sbin:/usr/local/go/bin:$GOPATH/bin:$HOME/settings:$HOME/bin:$HOME/Library/Haskell/bin:/Applications/Emacs.app/Contents/MacOS/bin:/usr/local/bin:$PATH

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    alias ls='ls --color=auto'
elif [[ "$unamestr" == 'Darwin' ]]; then
    alias ls='ls -G'
fi

export PS1='\h:\w $ '
