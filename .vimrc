set nocompatible
filetype off

" --- Vundles -----------------------------------------------------------------
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'vim-scripts/taglist.vim'
map <F3> :TlistToggle<cr>
Bundle 'vim-scripts/SearchComplete'
Bundle 'tpope/vim-surround'
Bundle 'vim-scripts/matchit.zip'
Bundle 'millermedeiros/vim-statline'
let g:statline_trailing_space=0
"Bundle 'scrooloose/syntastic'
Bundle 'Townk/vim-autoclose'
Bundle 'buftabs'
Bundle 'tpope/vim-markdown'
Bundle 'othree/html5.vim'
"Bundle 'vim-scripts/php.vim'
"Bundle 'jnwhiteh/vim-golang'
"autocmd FileType go autocmd BufWritePre <buffer> Fmt
Bundle 'vim-scripts/UltiSnips'
Bundle 'scrooloose/syntastic'
let g:syntastic_python_checkers=['pylint', 'pyflakes']
let g:syntastic_always_populate_loc_list=1
"Bundle 'kovisoft/slimv'
"let g:slimv_swank_cmd = '!osascript -e "tell application \"Terminal\" to do script \"sbcl --load ~/.vim/slime/start-swank.lisp\""'

" JSON
autocmd BufNewFile,BufRead *.json set ft=javascript

" --- Otras configuraciones ---------------------------------------------------
filetype plugin indent on
set background=dark
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
set textwidth=80
"set colorcolumn=80
set hidden
set guifont=Monaco:h12
set foldmethod=indent
set foldlevel=99

"make vim save and load the folding of the document each time it loads"
""also places the cursor in the last place that it was left."
au BufWinLeave * mkview
au BufWinEnter * silent loadview

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

noremap <S-left> :bprev<CR>
noremap <S-right> :bnext<CR>
