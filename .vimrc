set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'flazz/vim-colorschemes'
"Plugin 'godlygeek/csapprox'
Plugin 'vim-scripts/matchit.zip'
Plugin 'jiangmiao/auto-pairs'
Plugin 'kien/ctrlp.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'bling/vim-airline'
Plugin 'majutsushi/tagbar'
Plugin 'scrooloose/nerdtree'
"Plugin 'ervandew/supertab'
"Plugin 'SirVer/ultisnips'
"Plugin 'honza/vim-snippets'
Plugin 'Valloric/YouCompleteMe'
Plugin 'othree/html5.vim'
Plugin 'wting/rust.vim'
call vundle#end()
filetype plugin indent on

set t_Co=256
colorscheme jellybeans
colorscheme twilight256
"colorscheme Monokai
set laststatus=2

nmap <F8> :TagbarToggle<CR>
nmap <F7> :NERDTreeToggle<CR>
let g:UltiSnipsExpandTrigger="<C-j>"

let g:ycm_global_ycm_extra_conf = "~/.vim/ycm_extra_conf.py"
let g:ycm_key_list_select_completion=[]
let g:ycm_key_list_previous_completion=[]
let g:ycm_confirm_extra_conf = 0

syntax enable
set autoindent
set autoread
set nobackup
set noswapfile
set cursorline
"set cursorcolumn
set number
set encoding=utf8
set ffs=unix,mac,dos
set incsearch
set nohlsearch
set smartcase
set lazyredraw
set linebreak
set showcmd
set noshowmatch
set showmode
set shiftwidth=4
set softtabstop=4
set tabstop=8
set expandtab
set smarttab
set smartindent
set noswapfile
set viminfo='100,f1
set visualbell
set wildmenu
set wildignore=*.o,*~,*.pyc,*.bak,*.swp
set nowrap
set textwidth=80
set colorcolumn=80
set autowriteall

"set guifont=Monospace:h9

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

" Move between buffers
noremap <S-left> :bprev<CR>
noremap <S-right> :bnext<CR>
