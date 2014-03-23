set nocompatible
filetype off

" --- Vundles -----------------------------------------------------------------
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'

Bundle 'vim-scripts/taglist.vim'
let Tlist_Use_Right_Window = 1
nnoremap <silent> <F8> :TlistToggle<CR>

Bundle 'scrooloose/nerdtree'
nnoremap <silent> <F7> :NERDTreeToggle<CR>

Bundle 'vim-scripts/SearchComplete'
Bundle 'tpope/vim-surround'
Bundle 'vim-scripts/matchit.zip'
Bundle 'Townk/vim-autoclose'

" Comment with gc
Bundle 'tomtom/tcomment_vim'

Bundle 'bling/vim-airline'
set laststatus=2

Bundle 'tpope/vim-markdown'
Bundle 'othree/html5.vim'
autocmd BufNewFile,BufRead *.json set ft=javascript

" Git
Bundle 'tpope/vim-fugitive'

"Bundle 'vim-scripts/php.vim'
"Bundle 'jnwhiteh/vim-golang'
"autocmd FileType go autocmd BufWritePre <buffer> Fmt

" Snippets -> https://github.com/SirVer/ultisnips
Bundle 'SirVer/UltiSnips'
Bundle 'honza/vim-snippets'

" Python -> https://github.com/davidhalter/jedi-vim
Bundle 'davidhalter/jedi-vim'

" Syntax checking
Bundle 'scrooloose/syntastic'
"let g:syntastic_python_checkers=['pylint', 'pyflakes']
let g:syntastic_python_checkers=['flake8']
let g:syntastic_always_populate_loc_list=1

" --- Otras configuraciones ---------------------------------------------------
filetype plugin indent on
set background=dark
syntax enable
colorscheme jellybeans
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
