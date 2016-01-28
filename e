#!/bin/bash
unamestr=`uname`
if [[ "$unamestr" == 'Linux' ]]; then
    params=()
    for p in "$@"; do
      if [ "$p" == "-n" ]; then
          params+=( "$p" )
      elif [ "${p:0:1}" == "+" ]; then
          params+=( "$p" )
      else
        params+=( "/ssh:`hostname -a`:"$(readlink -f $p) )
      fi
    done
    emacsclient -f ~/.emacs.d/remote-server "${params[@]}"
elif [[ "$unamestr" == 'Darwin' ]]; then
    emacsclient "$@"
fi
