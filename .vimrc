set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'flazz/vim-colorschemes'
Plugin 'vim-scripts/matchit.zip'
Plugin 'jiangmiao/auto-pairs'
Plugin 'kien/ctrlp.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'bling/vim-airline'
Plugin 'majutsushi/tagbar'
Plugin 'scrooloose/nerdtree'
"Plugin 'SirVer/ultisnips'
"Plugin 'honza/vim-snippets'
Plugin 'Valloric/YouCompleteMe'
Plugin 'othree/html5.vim'
Plugin 'wting/rust.vim'
Plugin 'vim-scripts/scons.vim'
call vundle#end()
filetype plugin indent on

colorscheme jellybeans
set laststatus=2
set makeprg=scons

syntax enable
set autoindent
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
set showmatch
set showmode
set shiftwidth=4
set softtabstop=4
set tabstop=4
set expandtab
set smarttab
set smartindent
set noswapfile
set viminfo='100,f1
set visualbell
set wildmenu
set wildignore=*.o,*~,*.pyc,*.bak,*.swp
set wrap
set textwidth=100
set colorcolumn=100

set guifont=Monaco:h12

" make vim save and load the folding of the document each time it loads
" also places the cursor in the last place that it was left.
au BufWinLeave * mkview
au BufWinEnter * silent loadview

" Automatically remove all trailing spaces
autocmd BufWritePre * :%s/\s\+$//e

" Redefine ficheros por extensión
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd BufNewFile,BufReadPost *.json set ft=javascript

 let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'

" ------------ Keys -------------------------------------------
nmap <F8> :TagbarToggle<CR>
nmap <F7> :NERDTreeToggle<CR>
"let g:UltiSnipsExpandTrigger="<C-j>"

let mapleader = ","

" Smart way to move between windows
nmap <silent> <A-Up> :wincmd k<CR>
nmap <silent> <A-Down> :wincmd j<CR>
nmap <silent> <A-Left> :wincmd h<CR>
nmap <silent> <A-Right> :wincmd l<CR>

" Move between buffers
noremap <S-left> :bprev<CR>
noremap <S-right> :bnext<CR>
