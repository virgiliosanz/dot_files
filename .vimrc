set nocompatible
filetype off

"""""" Plugins
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'flazz/vim-colorschemes'
Plugin 'jiangmiao/auto-pairs'
Plugin 'kien/ctrlp.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'bling/vim-airline'
Plugin 'majutsushi/tagbar'
Plugin 'scrooloose/nerdtree'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'Valloric/YouCompleteMe'
Plugin 'Chiel92/vim-autoformat'
Plugin 'vim-scripts/Smart-Tabs'
Plugin 'othree/html5.vim'
Plugin 'fisadev/FixedTaskList.vim'   " Pending tasks list
Plugin 'godlygeek/tabular'
Plugin 'fatih/vim-go'
Plugin 'moll/vim-node'
Plugin 'maksimr/vim-jsbeautify'
Plugin 'einars/js-beautify'
call vundle#end()
filetype plugin indent on

"""""" Colors & GUI
set t_Co=256
"colorscheme gardener
colorscheme zenburn
syntax enable
set guifont=Monospace:h10

"""""" Generic Configuration
set laststatus=2
set autoindent
set autoread
set nobackup
set noswapfile
"set cursorline
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
"set smarttab
set noswapfile
set viminfo='100,f1
set visualbell
set wildmenu
set wildignore=*.o,*~,*.pyc,*.bak,*.swp
set nowrap
set textwidth=80
"set colorcolumn=80
set autowriteall
set smartindent
set noexpandtab
set copyindent
set preserveindent
set softtabstop=0
set shiftwidth=8
set tabstop=8
set cindent


" Make vim save and load the folding of the document each time it loads
" also places the cursor in the last place that it was left.
au BufWinLeave * mkview
au BufWinEnter * silent loadview

""""""" Automatically remove all trailing spaces
autocmd BufWritePre * :%s/\s\+$//e

""""""" Redefine ficheros por extensión
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd BufNewFile,BufReadPost *.json set ft=javascript

""""""" Indentantion by file type
autocmd Filetype html       setlocal ts=2 sw=2 expandtab
autocmd Filetype ruby       setlocal ts=4 sw=4 expandtab
autocmd Filetype python     setlocal ts=4 sw=4 expandtab
autocmd Filetype javascript setlocal ts=4 sw=4 sts=0 noexpandtab
autocmd Filetype c          setlocal ts=8 sw=8 sts=0 noexpandtab
autocmd Filetype cpp        setlocal ts=4 sw=4 sts=0 expandtab

""""""" YouCompleteMe conf
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

""""""" Autoformat conf
let g:formatprg_c = "astyle"
let g:formatprg_args_c = "--mode=c --style=kr -t "
let g:formatprg_args_cpp = "--mode=c --style=stroustrup -t4 "
autocmd FileType c map <buffer> <F3> :Autoformat<CR><CR>
"noremap <F3> :Autoformat<CR><CR>

""""""" Go
autocmd FileType go nmap <Leader>d <Plug>(go-doc)
autocmd FileType go nmap <Leader>s <Plug>(go-implements)
let g:go_fmt_command = "goimports"
let g:go_highlight_functions = 1
let g:go_highlight_methods = 1
let g:go_highlight_structs = 1
let g:go_highlight_operators = 1
let g:go_highlight_build_constraints = 1

""""""" Lua

""""""" node / javascript
autocmd Filetype javascript setlocal ts=4 sw=4 sts=0 noexpandtab
"" jsbeautifier
autocmd FileType javascript noremap <buffer> <F3> :call JsBeautify()<cr>
autocmd FileType html noremap <buffer> <F3> :call HtmlBeautify()<cr>
autocmd FileType css noremap <buffer> <F3> :call CSSBeautify()<cr>

""""""" Generic Keys
let mapleader = ","
nmap <F7> :NERDTreeToggle<CR>
nmap <F8> :TagbarToggle<CR>
nmap <F9> :TaskList<CR>
let g:UltiSnipsExpandTrigger="<C-j>"


" Move between buffers
noremap <S-left> :bprev<CR>
noremap <S-right> :bnext<CR>

""" http://vim.wikia.com/wiki/Automatically_open_the_quickfix_window_on_:make
" Automatically open, but do not go to (if there are errors) the quickfix /
" location list window, or close it when is has become empty.
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow
