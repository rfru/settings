#!/bin/bash
if [ ! -n "$1" ]
then
  echo "Usage: `basename $0` server"
  exit
fi
ssh -oNumberOfPasswordPrompts=0 $1 "echo Successful connection."
if [ $? -gt 0 ]
then
    exit
fi
# Try to copy the scripts.
export LC_MOUNT=/mnt/$1

REMOTE_DIR=`dirname $0`/remote
BIN_DIR=$REMOTE_DIR/bin
cat `dirname $0`/templates/coproc-echo | sed -e "s,\$LC_MOUNT,$LC_MOUNT,g" > $BIN_DIR/coproc-echo
chmod +x $BIN_DIR/coproc-echo
scp -r $REMOTE_DIR/* $1:~/
scp -r $REMOTE_DIR/.[^.]* $1:~/
rm $BIN_DIR/coproc-echo

# Mount the drives.
ds $1
mkdir -p $LC_MOUNT
sshfs -o Ciphers=arcfour -o Compression=no $1:/google /google 2>/dev/null
sshfs -o Ciphers=arcfour -o Compression=no $1:/usr/local/google /usr/local/google 2>/dev/null
sshfs -o Ciphers=arcfour -o Compression=no $1:/ $LC_MOUNT 2>/dev/null
