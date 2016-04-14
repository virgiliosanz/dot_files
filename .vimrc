set nocompatible
filetype off

" ------------ plugins -------------------------------------------
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'flazz/vim-colorschemes'
Plugin 'bling/vim-airline'

Plugin 'ctrlpvim/ctrlp.vim'
Plugin 'vim-scripts/Smart-Tabs'
Plugin 'jiangmiao/auto-pairs'
Plugin 'terryma/vim-multiple-cursors'

Plugin 'scrooloose/nerdtree'
Plugin 'majutsushi/tagbar'
Plugin 'vim-scripts/TaskList.vim'

Plugin 'SirVer/ultisnips'
" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<c-j>"
Plugin 'honza/vim-snippets'

Plugin 'tpope/vim-fugitive'

"Plugin 'scrooloose/syntastic'
"let g:syntastic_cpp_cpplint_exec = 'cpplint'
"let g:syntastic_cpp_check_header = 1
"let g:syntastic_always_populate_loc_list = 1
"let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 1
"let g:syntastic_check_on_wq = 1

Plugin 'Valloric/YouCompleteMe'
let g:ycm_autoclose_preview_window_after_completion = 1
let g:ycm_confirm_extra_conf = 0
"let g:ycm_global_ycm_extra_conf = "~/.vim/ycm_extra_conf.py"
"let g:ycm_global_ycm_extra_conf = ""
let g:ycm_seed_identifiers_with_syntax = 1
let g:ycm_add_preview_to_completeopt = 1
let g:ycm_autoclose_preview_window_after_completion = 1

""To see where the logfiles are, call :YcmDebugInfo.
"let g:ycm_server_keep_logfiles = 1
"let g:ycm_server_log_level = 'debug' " - debug - info - warning - error - critical
autocmd FileType c nnoremap <buffer> <silent> <C-]> :YcmCompleter GoTo<cr>
autocmd FileType c nnoremap <buffer> <silent> <F4> :YcmCompleter GoTo<CR>
autocmd FileType c nnoremap <buffer> <silent> <F6> :YcmCompleter GoToReferences<CR>
"map <leader>g  :YcmCompleter GoToDefinitionElseDeclaration<CR>
" Come back: Ctrl+O


Plugin 'Chiel92/vim-autoformat'
let g:formatprg_c = "astyle"
let g:formatprg_args_c = " --style=stroustrup "
noremap <F3> :Autoformat<CR><CR>

Plugin 'othree/html5.vim'
Plugin 'vim-scripts/indentpython.vim'

call vundle#end()

filetype plugin indent on

" ------------ global -------------------------------------------
set t_Co=256
"colorscheme gardener
"colorscheme zenburn
colorscheme jellybeans
"colorscheme twilighted
syntax enable
set guifont=Monospace:h10

set laststatus=2
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
set wrap
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

" For spell
"setlocal spell spelllang=en

" Make vim save and load the folding of the document each time it loads
" also places the cursor in the last place that it was left.
au BufWinLeave * mkview
au BufWinEnter * silent loadview

"" Automatically remove all trailing spaces
autocmd BufWritePre * :%s/\s\+$//e

"" Redefine ficheros por extensión
au BufNewFile,BufReadPost *.md set filetype=markdown
au BufNewFile,BufReadPost *.json set filetype=javascript
au BufNewFile,BufReadPost *.ino set filetype=cpp

au BufNewFile,BufRead *.c, *.h, *.cpp, *.cc
    \ set tabstop=4
    \ set softtabstop=4
    \ set shiftwidth=4
    \ set textwidth=79
    \ set expandtab
    \ set autoindent
    \ set fileformat=unix

au BufNewFile,BufRead *.py
    \ set tabstop=4
    \ set softtabstop=4
    \ set shiftwidth=4
    \ set textwidth=79
    \ set expandtab
    \ set autoindent
    \ set fileformat=unix

"Python with virtualenv support
py << EOF
import os
import sys
if 'VIRTUAL_ENV' in os.environ:
  project_base_dir = os.environ['VIRTUAL_ENV']
  activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
  execfile(activate_this, dict(__file__=activate_this))
EOF
let python_highlight_all=1


autocmd Filetype javascript setlocal ts=4 sw=4 sts=0 noexpandtab
au BufNewFile,BufRead *.js, *.html, *.css
    \ set tabstop=2
    \ set softtabstop=2
    \ set shiftwidth=2

"" Generic Keys
let mapleader = ","
nmap <F7> :NERDTreeToggle<CR>
nmap <F8> :TagbarToggle<CR>
nmap <F9> :TaskList<CR>

" Move between buffers
noremap <S-left> :bprev<CR>
noremap <S-right> :bnext<CR>

" window resizing with plus/minus keys
if bufwinnr(1)
	map + <C-W>+
	map - <C-W>-
endif

" ------------ Make -----------------------------------------
""" http://vim.wikia.com/wiki/Automatically_open_the_quickfix_window_on_:make
" Automatically open, but do not go to (if there are errors) the quickfix /
" location list window, or close it when is has become empty.
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

" ------------ C++ -------------------------------------------
let g:cpp_class_scope_highlight = 1
let g:cpp_experimental_template_highlight = 1
" If in cpp an press S+K search using cppman
autocmd FileType cpp set keywordprg=cppman
