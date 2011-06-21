#! /bin/bash

# This symlinks the dot emacs in this package to the .emacs in your home 
# directory while maintaining a backup

SCRIPT_PATH="$( cd "$( dirname "$0" )" && pwd )"

DOT_EMACS=$HOME/.emacs
JOE_DOT_EMACS=$SCRIPT_PATH/dot-emacs.el
DO_LINK="YES"

# If we are already sym linked just bail from the script
if [ -h $DOT_EMACS ]
then
    if [ $JOE_DOT_EMACS = `readlink $DOT_EMACS` ]
    then
        DO_LINK="NO"
    fi
fi

# Only link if we need to
if [ "NO" != $DO_LINK ]
then
    # Backup existing emacs file
    if [ -f $DOT_EMACS ]
    then
        mv $DOT_EMACS $HOME/.emacs.orig-`date +"%Y-%m-%d-%T"`
    fi

    # Sym-link in the current file
    ln -s $JOE_DOT_EMACS $DOT_EMACS 
fi

