GENERAL
========

cd ~
git clone git://github.com/virgiliosanz/dot_files.git
ln -s dot_files/.* .
rm ~/.git

VIM
====
run the following:

$ curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

# Prueba a instalar https://github.com/aitjcize/cppman  si vas a programar en
# C/C++ en VIM. Tendrás ayuda haciendo Shift-K en cualquier keyword de tu código

EMACS
=====
My Configuration for spacemacs (I might move from 15+ years of vim usage)

BASH
=====
# Instala plugins y demas ejecutando bash-it
git clone https://github.com/revans/bash-it.git ~/.bash_it

# Por defecto en mi mac
bash-it enable plugin ssh python osx less-pretty-cat git base alias-completion
bash-it enable completion defaults git pip ssh system vagrant
bash-it enable alias general osx


Otros
======
# Configuración para gdb de:
# https://github.com/cyrus-and/gdb-dashboard


