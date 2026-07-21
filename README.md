# GENERAL

cd ~
git clone git://github.com/virgiliosanz/dot_files.git
ln -s dot_files/.\* .
rm ~/.git

# neovim

- Neovim is based on AstroVim
- mkdir ~/.config ; cd .config/ ; ln -s ~/Code/dot_files/.config/nvim/ .

# BASH

## Starship

follow starship install instructions at: https://starship.rs/


## FISH

If you want to use fish instead... do:

 cp config.fish ~/.config/fish/config.fish

## Some alias and modern tooling

 alias cat="bat -pp"
 alias more=bat
 alias less=bat
 alias grep="rg -p"
 alias ls='eza --icons --group-directories-first'
