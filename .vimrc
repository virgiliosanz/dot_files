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
"Plugin 'SirVer/ultisnips'
"Plugin 'honza/vim-snippets'
Plugin 'Valloric/YouCompleteMe'
"Plugin 'othree/html5.vim'
"Plugin 'wting/rust.vim'
Plugin 'tomasr/molokai'
call vundle#end()
filetype plugin indent on

set t_Co=256
"colorscheme jellybeans
"colorscheme twilight256
"colorscheme Monokai
colorscheme molokai
let g:molokai_original = 1
let g:rehash256 = 1

set laststatus=2

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
set tabstop=8
set softtabstop=8
set shiftwidth=8
set noexpandtab
"set smarttab
"set smartindent
set viminfo='100,f1
set visualbell
set wildmenu
set wildignore=*.o,*~,*.pyc,*.bak,*.swp
set nowrap
"set textwidth=100
set colorcolumn=110
set autowriteall
set exrc
set secure

" make vim save and load the folding of the document each time it loads
" also places the cursor in the last place that it was left.
au BufWinLeave * mkview
au BufWinEnter * silent loadview

" Automatically remove all trailing spaces
autocmd BufWritePre * :%s/\s\+$//e

" Redefine ficheros por extensión
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd BufNewFile,BufReadPost *.json set ft=javascript


let g:ycm_seed_identifiers_with_syntax = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_autoclose_preview_window_after_completion = 1
nnoremap <leader>h :YcmCompleter GoToDeclaration<CR>
nnoremap <leader>d :YcmCompleter GoToDefinitionElseDeclaration<CR>
nnoremap <leader>i :YcmCompleter GoToDefinition<CR>


let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'
"let g:ycm_key_list_select_completion=[]
"let g:ycm_key_list_previous_completion=[]
let g:ycm_confirm_extra_conf = 0


" ------------ Keys -------------------------------------------
nmap <F8> :TagbarToggle<CR>
nmap <F7> :NERDTreeToggle<CR>
let g:UltiSnipsExpandTrigger="<C-j>"

let mapleader = ","

" Move between buffers
noremap <S-left> :bprev<CR>
noremap <S-right> :bnext<CR>
