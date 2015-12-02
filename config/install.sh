#!/bin/bash
unamestr=`uname`
cmd='greadlink'
if [[ "$unamestr" == 'Linux' ]]; then
    cmd='readlink'
fi
command -v $cmd >/dev/null 2>&1 || {
    sudo easy_install pip
    sudo pip install jedi epc
    brew install coreutils
    brew install bash-completion
    brew install the_silver_searcher
    exit;
}

ln -sf `$cmd -f .gitconfig` ~
ln -sf `$cmd -f .bash_profile` ~
ln -sf `$cmd -f .bashrc` ~
ln -sf `$cmd -f .Renviron` ~
ln -sf ~/.bash_profile ~/.profile

mkdir ~/.emacs.d
init=`$cmd -f emacs/init.el`
mcl=`$cmd -f emacs/mc-lists.el`
mkdir -p ~/.emacs.d
ln -sf $init ~/.emacs.d/
ln -sf $mcl ~/.emacs.d/.mc-lists.el

mkdir -p ~/.ssh
ln -s ~/Dropbox/keys/id_rsa ~/.ssh/id_rsa
