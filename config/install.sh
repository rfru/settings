#!/bin/bash
unamestr=`uname`
cmd='greadlink'
if [[ "$unamestr" == 'Linux' ]]; then
    cmd='readlink'
fi
command -v $cmd >/dev/null 2>&1 || {
    echo "Readlink needs to be installed.";
    brew install coreutils
    exit;
}

ln -sf `$cmd -f .gitconfig` ~
ln -sf `$cmd -f .bash_profile` ~
ln -sf ~/.bash_profile ~/.profile

init=`$cmd -f emacs/init.el`
mcl=`$cmd -f emacs/mc-lists.el`
mkdir -p ~/.emacs.d
ln -sf $init ~/.emacs.d/
ln -sf $mcl ~/.emacs.d/.mc-lists.el
