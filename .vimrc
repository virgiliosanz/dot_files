filetype off

" --- Vundles -----------------------------------------------------------------
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'

Bundle 'flazz/vim-colorschemes'
colorscheme jellybeans

Bundle 'vim-scripts/SearchComplete'
Bundle 'tpope/vim-surround'
Bundle 'vim-scripts/matchit.zip'
Bundle 'jiangmiao/auto-pairs'
Bundle 'kien/ctrlp.vim'
Bundle 'tpope/vim-fugitive'
Bundle 'bling/vim-airline'
set laststatus=2
" Use gc to comment/uncomment
Bundle 'tomtom/tcomment_vim'

Bundle 'majutsushi/tagbar'
nmap <F8> :TagbarToggle<CR>
Bundle 'scrooloose/nerdtree'
nnoremap <silent> <F7> :NERDTreeToggle<CR>

""" Snippets
"" Ctr+j -> exec snip
Bundle 'SirVer/ultisnips'
let g:UltiSnipsExpandTrigger="<C-j>"
Bundle 'honza/vim-snippets'

"" Supertab
"Bundle 'ervandew/supertab'

Bundle 'scrooloose/syntastic'
let g:syntastic_always_populate_loc_list=1
"let g:syntastic_python_checkers=['pylint', 'pyflakes']
let g:syntastic_python_checkers=['flake8']
let g:flake8_ignore="E302"

Bundle 'othree/html5.vim'
autocmd BufNewFile,BufRead *.json set ft=javascript

" GO Lang:
"  , + i -> Import under cursor
"  , + d -> Goto Definition
"  , + gd -> Goto Doc
" C-x + C-o -> Completion
"Bundle 'fatih/vim-go'
"let g:go_snippet_engine = "ultisnips"
"au Filetype go nnoremap <buffer> <leader>i :exe 'GoImport ' . expand('<cword>')<CR>
"au FileType go nmap <Leader>d <Plug>(go-def-split)
"au FileType go nmap <Leader>gd <Plug>(go-doc)
""au FileType go nmap gd <Plug>(go-def)
""au FileType go nmap <Leader>ds <Plug>(go-def-split)
""au FileType go nmap <Leader>dv <Plug>(go-def-vertical)
""au FileType go nmap <Leader>dt <Plug>(go-def-tab)
"let g:go_fmt_autosave = 1
"let g:go_highlight_array_whitespace_error = 1
"let g:go_highlight_chan_whitespace_error = 1
"let g:go_highlight_extra_types = 1
"let g:go_auto_type_info = 1

"Bundle 'jimenezrick/vimerl'

"" Python
"Bundle 'klen/python-mode'
Bundle 'davidhalter/jedi-vim'
" Para hacer REPL desde vim
"Bundle 'ivanov/vim-ipython'
" Like Slime for Vim
" https://github.com/epeli/slimux

"" Scala
"Bundle 'derekwyatt/vim-scala'

"" Rust
Bundle 'wting/rust.vim'


" --- Otras configuraciones ---------------------------------------------------
filetype plugin indent on
syntax enable
"set background=dark
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
"set nowrap
set wrap
set textwidth=100
set colorcolumn=100
"set hidden

"------- Para GUI
set guifont=Monaco:h12
set lines=40
set columns=110

" make vim save and load the folding of the document each time it loads
" also places the cursor in the last place that it was left.
au BufWinLeave * mkview
au BufWinEnter * silent loadview

" Automatically remove all trailing spaces
autocmd BufWritePre * :%s/\s\+$//e

" ------ Redefine ficheros por extensión
autocmd BufNewFile,BufReadPost *.md set filetype=markdown

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
