export CONAN_REVISIONS_ENABLED=1
export CONAN_CPU_COUNT=2
export LANG='en_US.UTF-8'
export LC_ALL="en_US.UTF-8"
export GOPATH=$HOME/go
export PATH=/usr/local/sbin:/usr/local/go/bin:$GOPATH/bin:$HOME/settings:$HOME/bin:$HOME/.local/bin:/Applications/Emacs.app/Contents/MacOS/bin:/usr/local/bin:$PATH

unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    alias ls='ls --color=auto'
    # Disable middle mouse button
    xmodmap -e 'pointer = 1 25 3 4 5 6 7 8 9 10 11 12'
elif [[ "$unamestr" == 'Darwin' ]]; then
    alias ls='ls -G'
fi

export PS1='\h:\w $ '
