#!/bin/sh

if test ! -d $HOME/.emacs.d
then
  echo "  Installing emacs prelude for you."
  git clone https://github.com/bbatsov/prelude.git $HOME/.emacs.d
fi

epremo_path=$HOME/.emacs.d/prelude-modules.el
if test ! -L $epremo_path
then
  if test -f $epremo_path
  then
    rm $epremo_path
  fi
  echo "  Installing your personal modules config for emacs prelude."
  ln -s $ZSH/emacs/prelude-modules.el $epremo_path
fi

eper_path=$HOME/.emacs.d/personal
if test ! -L $eper_path
then
  if test -d $eper_path
  then
    rm -rf $eper_path
  fi
  echo "  Installing your personal modules config for emacs prelude."
  ln -s $ZSH/emacs/personal $eper_path
fi

