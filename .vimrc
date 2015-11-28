set nocompatible
filetype off

" ------------ plugins -------------------------------------------
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
Plugin 'fatih/vim-go'
" IMPORTANT NOTE: nsf/gocode is what does the autocompletion.
Plugin 'nsf/gocode', {'rtp': 'vim/'}

call vundle#end()
filetype plugin indent on

" ------------ global -------------------------------------------
set t_Co=256
"colorscheme zenburn
colorscheme jellybeans
"colorscheme Monokai
set laststatus=2

syntax enable
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

set guifont=Monospace:h10

" make vim save and load the folding of the document each time it loads
" also places the cursor in the last place that it was left.
au BufWinLeave * mkview
au BufWinEnter * silent loadview

" Automatically remove all trailing spaces
autocmd BufWritePre * :%s/\s\+$//e

" Redefine ficheros por extensión
autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd BufNewFile,BufReadPost *.json set ft=javascript

""" http://vim.wikia.com/wiki/Automatically_open_the_quickfix_window_on_:make
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

" ------------ Keys -------------------------------------------
" F3 -> Autoformat C style
noremap <F3> :Autoformat<CR><CR>
let mapleader = ","

nmap <F8> :TagbarToggle<CR>
nmap <F7> :NERDTreeToggle<CR>

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

" ------------ Snippets -------------------------------------------
" Trigger configuration. Do not use <tab> if you use
" https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-h>"
let g:UltiSnipsJumpForwardTrigger="<c-n>"
let g:UltiSnipsJumpBackwardTrigger="<c-m>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"


" ------------ C -------------------------------------------
" Autoformat conf
let g:formatprg_c = "astyle"
let g:formatprg_args_c = "--mode=c --style=knf -t "


" ------------ Go -------------------------------------------
let g:go_fmt_command = "goimports"
map <F4> :GoDef<CR>
map <F5> :GoRename<CR>
map <F6> :GoReferrers<CR>
let g:tagbar_type_go = {
    			\ 'ctagstype' : 'go',
    			\ 'kinds'     : [
        		\ 'p:package',
        		\ 'i:imports:1',
        		\ 'c:constants',
        		\ 'v:variables',
        		\ 't:types',
        		\ 'n:interfaces',
        		\ 'w:fields',
        		\ 'e:embedded',
        		\ 'm:methods',
        		\ 'r:constructor',
        		\ 'f:functions'
    			\ ],
    			\ 'sro' : '.',
    			\ 'kind2scope' : {
        		\ 't' : 'ctype',
        		\ 'n' : 'ntype'
    			\ },
    			\ 'scope2kind' : {
        		\ 'ctype' : 't',
        		\ 'ntype' : 'n'
    			\ },
    			\ 'ctagsbin'  : 'gotags',
    			\ 'ctagsargs' : '-sort -silent'
			\ }

