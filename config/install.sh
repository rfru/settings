#!/bin/bash
init=`readlink -f emacs/init.el`
mcl=`readlink -f emacs/mc-lists.el`
mkdir -p ~/.emacs.d
ln -sf $init ~/.emacs.d/
ln -sf $mcl ~/.emacs.d/.mc-lists.el
