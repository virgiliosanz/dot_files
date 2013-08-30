set nocompatible
filetype off

" --- Vundles -----------------------------------------------------------------
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'scrooloose/nerdtree'
map <F2> :NERDTreeToggle<cr>
Bundle 'vim-scripts/taglist.vim'
map <F3> :TlistToggle<cr>
Bundle 'vim-scripts/SearchComplete'
Bundle 'tpope/vim-surround'
Bundle 'vim-scripts/matchit.zip'
Bundle 'tomtom/checksyntax_vim'
Bundle 'millermedeiros/vim-statline'
let g:statline_trailing_space=0
Bundle 'scrooloose/syntastic'
autocmd FileType go autocmd BufWritePre <buffer> Fmt
Bundle 'Townk/vim-autoclose'
Bundle 'buftabs'
Bundle 'tpope/vim-markdown'
Bundle 'othree/html5.vim'
Bundle 'vim-scripts/php.vim'
Bundle "jnwhiteh/vim-golang"

Bundle 'altercation/vim-colors-solarized'
let g:solarized_termcolors=256

" JSON
autocmd BufNewFile,BufRead *.json set ft=javascript

" --- Otras configuraciones ---------------------------------------------------
filetype plugin indent on
set background=dark
syntax enable
colorscheme solarized
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
set textwidth=80
set colorcolumn=80
set hidden
set guifont=Monaco:h12

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

noremap <S-left> :bprev<CR>
noremap <S-right> :bnext<CR>

" Brief help  -----------------------------------------------------------------
" :BundleList          - list configured bundles
" :BundleInstall(!)    - install(update) bundles
" :BundleSearch(!) foo - search(or refresh cache first) for foo
" :BundleClean(!)      - confirm(or auto-approve) removal of unused bundles
"
" see :h vundle for more details or wiki for FAQ
" NOTE: comments after Bundle command are not allowed..
