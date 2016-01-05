set nocompatible
filetype off

" ------------ plugins -------------------------------------------
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'gmarik/Vundle.vim'
Plugin 'flazz/vim-colorschemes'
Plugin 'jiangmiao/auto-pairs'
Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'bling/vim-airline'
Plugin 'majutsushi/tagbar'
Plugin 'scrooloose/nerdtree'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'Valloric/YouCompleteMe'
Plugin 'octol/vim-cpp-enhanced-highlight'
Plugin 'Chiel92/vim-autoformat'
Plugin 'vim-scripts/Smart-Tabs'
Plugin 'xolox/vim-misc'
Plugin 'xolox/vim-easytags'
call vundle#end()
filetype plugin indent on

" ------------ global -------------------------------------------
set t_Co=256
"colorscheme gardener
"colorscheme zenburn
colorscheme jellybeans
syntax enable
set guifont=Monospace:h10

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

"set guifont=Monospace:h10

" Make vim save and load the folding of the document each time it loads
" also places the cursor in the last place that it was left.
au BufWinLeave * mkview
au BufWinEnter * silent loadview

""""""" Automatically remove all trailing spaces
autocmd BufWritePre * :%s/\s\+$//e

""""""" Redefine ficheros por extensión
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd BufNewFile,BufReadPost *.json set ft=javascript

autocmd Filetype html       setlocal ts=2 sw=2 expandtab
autocmd Filetype ruby       setlocal ts=4 sw=4 expandtab
autocmd Filetype python     setlocal ts=4 sw=4 expandtab
autocmd Filetype javascript setlocal ts=4 sw=4 sts=0 noexpandtab
autocmd Filetype c          setlocal ts=8 sw=8 sts=0 noexpandtab
autocmd Filetype cpp        setlocal ts=4 sw=4 sts=0 expandtab

""""""" YouCompleteMe conf
""" http://vim.wikia.com/wiki/Automatically_open_the_quickfix_window_on_:make
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

" ------------ Keys -------------------------------------------
let mapleader = ","
noremap <F3> :Autoformat<CR><CR>

nmap <F7> :NERDTreeToggle<CR>
nmap <F8> :TagbarToggle<CR>

" Move between buffers
noremap <S-left> :bprev<CR>
noremap <S-right> :bnext<CR>

" ------------ completion -------------------------------------------
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
let g:ycm_add_preview_to_completeopt = 0

""""""" Autoformat conf
let g:formatprg_c = "astyle"
let g:formatprg_args_c = "--mode=c --style=linux -T "
autocmd FileType c map <buffer> <F3> :Autoformat<CR><CR>

let g:formatprg_args_cpp = "--mode=c --style=stroustrup -t4 "
autocmd FileType cpp map <buffer> <F3> :Autoformat<CR><CR>
"noremap <F3> :Autoformat<CR><CR>

" Create tags with: ctags -R .
" Ctrl+] -> Goto Def Ctrl+T go back from def
" S+K -> Search manpage under cursor

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

" ------------ Snippets -------------------------------------------
" Trigger configuration. Do not use <tab> if you use
" https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-h>"
let g:UltiSnipsJumpForwardTrigger="<c-n>"
let g:UltiSnipsJumpBackwardTrigger="<c-m>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"


" ------------ C++ -------------------------------------------
let g:cpp_class_scope_highlight = 1
let g:cpp_experimental_template_highlight = 1
autocmd FileType cpp set keywordprg=cppman

"" ------------ Go -------------------------------------------
"let g:go_fmt_command = "goimports"
"let g:go_highlight_functions = 1
"let g:go_highlight_methods = 1
"let g:go_highlight_structs = 1
"let g:go_highlight_operators = 1
"let g:go_highlight_build_constraints = 1
"
"autocmd FileType go map <F4> :GoDef<CR>
"autocmd FileType go map <F5> :GoRename<CR>
"autocmd FileType go map <F6> :GoReferrers<CR>
