#!/bin/sh

if test -d $HOME/.vim
then
  echo "Backing up vim folder"
  mv $HOME/.vim $HOME/.vim.old
  rm -rf $HOME/.vim
fi

if test -L $HOME/.vim
then
  echo ".vim is a symlink, removing it"
  rm $HOME/.vim
fi

if test -e $HOME/.vimrc
then
  echo "Backing up vimrc"
  mv $HOME/.vimrc $HOME/.vimrc.old
  rm $HOME/.vimrc
fi

if test -L $HOME/.vimrc
then
  echo ".vimrc is a symlink, removing it"
  rm $HOME/.vimrc
fi

DOTFILES_ROOT="`pwd`"

cp -R $DOTFILES_ROOT/vim ~/.vim
ln -s ~/.vim/vimrc ~/.vimrc

if test ! -d $HOME/.vim/bundle/vundle
then
  git clone https://github.com/gmarik/vundle.git ~/.vim/bundle/vundle
fi

echo "Finished installing vim settings"
