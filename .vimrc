set nocompatible
filetype off

" --- Vundles -----------------------------------------------------------------
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'vim-scripts/SearchComplete'
Bundle 'tpope/vim-surround'
Bundle 'vim-scripts/matchit.zip'
Bundle 'jiangmiao/auto-pairs'
Bundle 'majutsushi/tagbar'
nmap <F8> :TagbarToggle<CR>
Bundle 'scrooloose/nerdtree'
nnoremap <silent> <F7> :NERDTreeToggle<CR>

Bundle 'altercation/vim-colors-solarized'
let g:solarized_termtrans=1
let g:solarized_contrast="normal"
colors solarized

Bundle 'bling/vim-airline'
set laststatus=2
Bundle 'SirVer/UltiSnips'
Bundle 'honza/vim-snippets'
Bundle 'Valloric/YouCompleteMe'
" Use gc to comment/uncomment
Bundle 'tomtom/tcomment_vim'
Bundle 'tpope/vim-fugitive'

Bundle 'scrooloose/syntastic'
let g:syntastic_always_populate_loc_list=1
"let g:syntastic_python_checkers=['pylint', 'pyflakes']
let g:syntastic_python_checkers=['flake8']

Bundle 'othree/html5.vim'
autocmd BufNewFile,BufRead *.json set ft=javascript

Bundle 'fatih/vim-go'
let g:go_snippet_engine = "ultisnips"
au Filetype go nnoremap <buffer> <leader>i :exe 'GoImport ' . expand('<cword>')<CR>

" --- Otras configuraciones ---------------------------------------------------
filetype plugin indent on
syntax enable
set background=dark
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
set nowrap
set textwidth=100
set colorcolumn=100
"set hidden
set guifont=Monaco:h12

" make vim save and load the folding of the document each time it loads
" also places the cursor in the last place that it was left.
au BufWinLeave * mkview
au BufWinEnter * silent loadview

" Automatically remove all trailing spaces
autocmd BufWritePre * :%s/\s\+$//e

" ------------ Keys -------------------------------------------
let mapleader = ","

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Move between buffers
noremap <S-left> :bprev<CR>
noremap <S-right> :bnext<CR>
