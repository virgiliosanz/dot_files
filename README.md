# GENERAL

cd ~
git clone git://github.com/virgiliosanz/dot_files.git
ln -s dot_files/.\* .
rm ~/.git

# neovim

- Neovim is based on AstroVim
- mkdir ~/.config ; cd .config/ ; ln -s ~/Code/dot_files/.config/nvim/ .

# BASH

# Instala plugins y demas ejecutando bash-it

git clone https://github.com/revans/bash-it.git ~/.bash_it

# Por defecto en mi mac

bash-it enable plugin ssh python osx less-pretty-cat git base alias-completion
bash-it enable completion defaults git pip ssh system vagrant
bash-it enable alias general osx

# FISH

If you want to use fish instead... do:

cp config.fish ~/.config/fish/config.fish
