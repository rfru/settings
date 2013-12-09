#!/bin/bash
ln -sf `greadlink -f .gitconfig` ~
ln -sf `greadlink -f .bash_profile` ~
ln -sf ~/.bash_profile ~/.profile

init=`greadlink -f emacs/init.el`
mcl=`greadlink -f emacs/mc-lists.el`
mkdir -p ~/.emacs.d
ln -sf $init ~/.emacs.d/
ln -sf $mcl ~/.emacs.d/.mc-lists.el
