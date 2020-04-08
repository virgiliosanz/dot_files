GENERAL
========

cd ~
git clone git://github.com/virgiliosanz/dot_files.git
ln -s dot_files/.* .
rm ~/.git

VIM
====
run for the first time and them PlugUpdate inside


# Prueba a instalar https://github.com/aitjcize/cppman  si vas a programar en
# C/C++ en VIM. Tendrás ayuda haciendo Shift-K en cualquier keyword de tu código


EMACS
=====
My Configuration for spacemacs (I might move from 20+ years of vi/vim usage)
install it: git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d then run emacs

BASH
=====
# Instala plugins y demas ejecutando bash-it
git clone https://github.com/revans/bash-it.git ~/.bash_it

# Por defecto en mi mac
bash-it enable plugin ssh python osx less-pretty-cat git base alias-completion
bash-it enable completion defaults git pip ssh system vagrant
bash-it enable alias general osx

FISH
=====
If you want to use fish instead... do:

cp config.fish ~/.config/fish/config.fish


Otros
======
# Configuración para gdb de:
# https://github.com/cyrus-and/gdb-dashboard


