#!/bin/bash
go get golang.org/x/tools/cmd/goimports
go get github.com/nsf/gocode

unamestr=`uname`
cmd='greadlink'
if [[ "$unamestr" == 'Linux' ]]; then
    cmd='readlink'
    command -v $cmd >/dev/null 2>&1 || {
        sudo aptitude install python-pip
        sudo pip install jedi epc
        sudo apt-get install silversearcher-ag
        exit;
    }
else
  command -v $cmd >/dev/null 2>&1 || {
      sudo easy_install pip
      sudo pip install jedi epc
      brew install coreutils
      brew install bash-completion
      brew install the_silver_searcher
      exit;
  }
fi

ln -sf `$cmd -f .gitconfig` ~
ln -sf `$cmd -f .bash_profile` ~
ln -sf `$cmd -f .bashrc` ~
ln -sf `$cmd -f .Renviron` ~
ln -sf ~/.bash_profile ~/.profile

mkdir ~/.emacs.d
init=`$cmd -f emacs/init.el`
mkdir -p ~/.emacs.d
ln -sf $init ~/.emacs.d/

mkdir -p ~/.ssh
ln -s ~/Dropbox/keys/id_rsa ~/.ssh/id_rsa
ln -s ~/Dropbox/keys/id_rsa.pub ~/.ssh/id_rsa.pub
ln -s ~/Dropbox/keys/config ~/.ssh/config
