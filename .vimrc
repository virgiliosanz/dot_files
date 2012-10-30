set nocompatible
filetype off

" --- Vundles -----------------------------------------------------------------
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
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
filetype plugin indent on
syntax enable
colorscheme jellybeans
set autoindent
set autoread
set nobackup
set cursorline
set cursorcolumn
set encoding=utf8
set expandtab
set ffs=unix,mac,dos
set gcr=a:blinkon0
set hlsearch
set incsearch
set laststatus=2
set lazyredraw
set linebreak
set magic
set noswapfile
set nowb
set number
set ruler
set shiftwidth=4
set showcmd
set showmatch
set showmode
set smarttab
set smartcase
set smartindent
set softtabstop=4
set so=7
set noswapfile
set tabstop=4
set viminfo='100,f1
set visualbell
set wildmenu
set wildignore=*.o,*~,*.pyc,*.bak,*.swp
set nowb
set nowrap

set hidden

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Brief help  -----------------------------------------------------------------
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install(update) bundles
" :BundleSearch(!) foo - search(or refresh cache first) for foo
" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle command are not allowed..
