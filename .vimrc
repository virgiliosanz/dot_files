set nocompatible
filetype off

" ------------ plugins -------------------------------------------
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

Plugin 'flazz/vim-colorschemes'
Plugin 'bling/vim-airline'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'vim-scripts/Smart-Tabs'
Plugin 'jiangmiao/auto-pairs'

Plugin 'scrooloose/nerdtree'
nmap <F7> :NERDTreeToggle<CR>

Plugin 'majutsushi/tagbar'
nmap <F8> :TagbarToggle<CR>

Plugin 'vim-scripts/TaskList.vim'
nmap <F9> :TaskList<CR>

Plugin 'chiel92/vim-autoformat'
nmap <F3> :Autoformat<CR>

"Plugin 'editorconfig/editorconfig-vim'
Plugin 'scrooloose/syntastic'
Plugin 'sheerun/vim-polyglot'
Plugin 'Valloric/YouCompleteMe'
Plugin 'honza/vim-snippets'
Plugin 'SirVer/ultisnips'

Plugin 'vim-scripts/Arduino-syntax-file'
Plugin 'slashmili/alchemist.vim'

call vundle#end()

filetype plugin indent on

" ------------ global -------------------------------------------
set guifont=Monaco:h12
set t_Co=256
"colorscheme ir_black
"colorscheme Molokai
"colorscheme wombat256
"colorscheme zenburn
colorscheme Jellybeans
syntax enable

set laststatus=2
set autoindent
set smartindent
set copyindent
set preserveindent
set softtabstop=0
set shiftwidth=8
set tabstop=8
set cindent
set autoread
set nobackup
set noswapfile
set cursorline
set cursorcolumn
set number
set encoding=utf8
set ffs=unix,mac,dos
set incsearch
set nohlsearch
set smartcase
set lazyredraw
set linebreak
set showcmd
"set noshowmatch
set showmode
set smarttab
set noswapfile
set viminfo='100,f1
set visualbell
set wildmenu
set wildignore=*.o,*~,*.pyc,*.bak,*.swp
set wrap
set textwidth=80
set colorcolumn=80
set autowriteall
set showbreak=↪

" For spell
"setlocal spell spelllang=en
"set spelllang=en
"set spell
set nospell

" Make vim save and load the folding of the document each time it loads
" also places the cursor in the last place that it was left.
au BufWinLeave * mkview
au BufWinEnter * silent loadview

"" Automatically remove all trailing spaces
au BufWritePre * :%s/\s\+$//e

"" Keys
let mapleader = ","
" Move between buffers
nmap <S-left> :bprev<CR>
nmap <S-right> :bnext<CR>
" window resizing with plus/minus keys
if bufwinnr(1)
    map + <C-W>+
    map - <C-W>-
endif


"" Redefine ficheros por extensión
au BufNewFile,BufReadPost *.json set filetype=javascript

" virtualenv support
py << EOF
import os
import sys
if 'VIRTUAL_ENV' in os.environ:
  project_base_dir = os.environ['VIRTUAL_ENV']
  activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
  execfile(activate_this, dict(__file__=activate_this))
EOF
let python_highlight_all=1

" ------------ Make -----------------------------------------
""" http://vim.wikia.com/wiki/Automatically_open_the_quickfix_window_on_:make
" Automatically open, but do not go to (if there are errors) the quickfix /
" location list window, or close it when is has become empty.
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow
