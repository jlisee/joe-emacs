#! /bin/bash

# This symlinks the dot emacs in this package to the .emacs in your home 
# directory while maintaining a backup

SCRIPT_PATH="$( cd "$( dirname "$0" )" && pwd )"

# Backup existing emacs file
if [ -f $HOME/.emacs ]
then
    mv $HOME/.emacs $HOME/.emacs.orig-`date +"%Y-%m-%d-%T"`
fi

# Sym-link in the current file
ln -s $SCRIPT_PATH/dot-emacs.el $HOME/.emacs 

