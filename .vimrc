set nocompatible
filetype off

set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'flazz/vim-colorschemes'
"Plugin 'godlygeek/csapprox'
"Plugin 'vim-scripts/matchit.zip'
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
Plugin 'Chiel92/vim-autoformat'
Plugin 'vim-scripts/Smart-Tabs'
"Plugin 'othree/html5.vim'
"Plugin 'wting/rust.vim'
call vundle#end()
filetype plugin indent on

set t_Co=256
colorscheme jellybeans
colorscheme twilight256
"colorscheme Monokai
set laststatus=2

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
set noshowmatch
set showmode
"set smarttab
set noswapfile
set viminfo='100,f1
set visualbell
set wildmenu
set wildignore=*.o,*~,*.pyc,*.bak,*.swp
set nowrap
set textwidth=80
set colorcolumn=80
set autowriteall
set smartindent

set noexpandtab
set copyindent
set preserveindent
set softtabstop=0
set shiftwidth=8
set tabstop=8
set cindent

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

" YouCompleteMe conf
let g:ycm_global_ycm_extra_conf = "~/.vim/ycm_extra_conf.py"
let g:ycm_key_list_select_completion=[]
let g:ycm_key_list_previous_completion=[]
let g:ycm_confirm_extra_conf = 0
let g:ycm_collect_identifiers_from_tags_files = 1
let g:ycm_seed_identifiers_with_syntax = 1
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_autoclose_preview_window_after_insertion = 1
"let g:ycm_add_preview_to_completeopt = 0
let g:ycm_key_list_select_completion=['<Enter>', '<Down>']
let g:ycm_key_list_previous_completion=[]

" Autoformat conf
let g:formatprg_c = "astyle"
let g:formatprg_args_c = "--mode=c --style=knf -t -l"

" ------------ Keys -------------------------------------------
" F3 -> Autoformat C style
noremap <F3> :Autoformat<CR><CR>

let mapleader = ","

nmap <F8> :TagbarToggle<CR>
nmap <F7> :NERDTreeToggle<CR>

let g:UltiSnipsExpandTrigger="<C-j>"


" Move between buffers
noremap <S-left> :bprev<CR>
noremap <S-right> :bnext<CR>
