#!/bin/bash
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    emacsclient -t -a "" "$@"
elif [[ "$unamestr" == 'Darwin' ]]; then
    open -a Emacs "$@"
fi
