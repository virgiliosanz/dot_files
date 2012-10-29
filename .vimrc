set nocompatible               " be iMproved
filetype off                   " required!

" --- Vundles -----------------------------------------------------------------
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" let Vundle manage Vundle
" required! 
Bundle 'gmarik/vundle'

" original repos on github
Bundle 'scrooloose/nerdtree'
map <F2> :NERDTreeToggle<cr>
Bundle 'msanders/snipmate.vim'
Bundle 'vim-scripts/taglist.vim'
map <F3> :TlistToggle<cr>
Bundle 'ervandew/supertab'
Bundle 'vim-scripts/SearchComplete'
Bundle 'tpope/vim-surround'
Bundle 'vim-scripts/matchit.zip'
Bundle 'vim-scripts/php.vim'
Bundle 'tomtom/checksyntax_vim'
Bundle 'millermedeiros/vim-statline'
let g:statline_trailing_space=0
Bundle 'scrooloose/syntastic'
Bundle 'Townk/vim-autoclose'
Bundle 'tpope/vim-markdown'
Bundle 'othree/html5.vim'
Bundle 'msanders/cocoa.vim'
Bundle 'buftabs'
noremap <S-left> :bprev<CR>
noremap <S-right> :bnext<CR>

" --- Otras configuraciones ---------------------------------------------------
filetype plugin indent on     " required!
syntax enable
colorscheme jellybeans
set autoindent
set autoread
set nobackup
"set colorcolumn=80
set cursorline
set expandtab
set gcr=a:blinkon0
set hlsearch
set incsearch
set laststatus=2
set linebreak
set noswapfile
set nowb
set number
set ruler
set shiftwidth=2
set showcmd
set showmode
set smartindent
set softtabstop=2
set tabstop=2
set viminfo='100,f1
set visualbell
set nowrap

set hidden

" Brief help  -----------------------------------------------------------------
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install(update) bundles
" :BundleSearch(!) foo - search(or refresh cache first) for foo
" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle command are not allowed..
