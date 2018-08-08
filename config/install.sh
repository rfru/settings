#!/bin/bash
sudo aptitude install -y python-pip libtool autoconf automake pkg-config
sudo pip install jedi epc
sudo apt-get install -y silversearcher-ag

ln -sf `$cmd -f .gitconfig` ~
ln -sf `$cmd -f .bash_profile` ~
ln -sf `$cmd -f .bashrc` ~
source .bashrc
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

go get golang.org/x/tools/cmd/goimports
go get github.com/nsf/gocode

