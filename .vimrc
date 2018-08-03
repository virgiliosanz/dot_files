set nocompatible

" ------------ plugins -------------------------------------------
" Load vim-plug
if empty(glob("~/.vim/autoload/plug.vim"))
    execute 'mkdir -p ~/.vim/plugged'
    execute 'mkdir -p ~/.vim/autoload'
    execute '!curl -fLo ~/.vim/autoload/plug.vim https://raw.github.com/junegunn/vim-plug/master/plug.vim'
endif

call plug#begin('~/.vim/plugged')

Plug 'junegunn/vim-plug'

Plug 'flazz/vim-colorschemes'
Plug 'bling/vim-airline'
let g:airline#extensions#tabline#enabled = 1

Plug 'brooth/far.vim'


Plug 'ctrlpvim/ctrlp.vim'
Plug 'vim-scripts/Smart-Tabs'
Plug 'jiangmiao/auto-pairs'
Plug 'scrooloose/nerdtree'

"Plug 'editorconfig/editorconfig-vim'
Plug 'chiel92/vim-autoformat'
Plug 'majutsushi/tagbar'
Plug 'sheerun/vim-polyglot'
Plug 'mileszs/ack.vim'
if executable('ag')
  let g:ackprg = 'ag --vimgrep'
endif

"Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'
"let g:UltiSnipsExpandTrigger = "<c-j>"

Plug 'w0rp/ale'
let g:airline#extensions#ale#enabled = 1

"" You Complete Me
"Plug 'rdnetto/YCM-Generator', { 'branch': 'stable'}
"Plug 'Valloric/YouCompleteMe', { 'do': 'python3 install.py --clang-completer' }
"let g:ycm_complete_in_comments=1
"let g:ycm_confirm_extra_conf=0
"let g:ycm_collect_identifiers_from_tags_files=1
"set completeopt=longest,menuone
"let g:ycm_min_num_of_chars_for_completion=2
"let g:ycm_cache_omnifunc=0
"let g:ycm_seed_identifiers_with_syntax=1
"let g:ycm_auto_trigger=1
""let g:ycm_server_use_vim_stdout = 0
""let g:ycm_server_keep_logfiles = 1

" Elixir
"Plug 'slashmili/alchemist.vim', { 'for': 'elixir' }

" Rust
"Plug 'rust-lang/rust.vim', {'for': 'rust' }
"let g:ycm_rust_src_path = '/opt/local/share/rust/src/'
"let g:rustfmt_autosave = 1
"let g:rust_recommended_style = 1

" golang
"Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }

" Python
" Active for python if Ycm is not being used
 Plug 'davidhalter/jedi-vim'
 let g:jedi#popup_select_first = 0
 let g:jedi#goto_command = "<leader>d"
 let g:jedi#goto_assignments_command = "<leader>g"
 let g:jedi#goto_definitions_command = ""
 let g:jedi#documentation_command = "K"
 let g:jedi#usages_command = "<leader>n"
 let g:jedi#completions_command = "<C-Space>"
 let g:jedi#rename_command = "<leader>r"

call plug#end()

filetype plugin indent on

" ------------ global -------------------------------------------
set guifont=Monaco:h12
set t_Co=256
"colorscheme ir_black
"colorscheme Molokai
"colorscheme wombat256
"colorscheme zenburn
"colorscheme Jellybeans
"colorscheme colorsbox-material
colorscheme badwolf

syntax enable

set laststatus=2
set autoindent
set smartindent
set copyindent
set preserveindent
set softtabstop=0
set shiftwidth=4
set tabstop=8
set cindent
set autoread
set nobackup
set noswapfile
set cursorline
set cursorcolumn
set rnu
set nu
set encoding=utf8
set ffs=unix,mac,dos
set incsearch
set nohlsearch
set ignorecase
set smartcase
set lazyredraw
set linebreak
set showcmd
"set noshowmatch
set noshowmode
set smarttab
set noswapfile
set viminfo='100,f1
set visualbell
set wildmenu
set wildmode=list:longest

set wildignore=*.o,*~,*.pyc,*.bak,*.swp
set wildignore+=**/node_modules/**
set wildignore+=**/.git/**
set wildignore+=**/.*/**

set wrap
set textwidth=80
set colorcolumn=80
set autowriteall
set showbreak=↪
" For spell
"setlocal spell spelllang=en
"set spelllang=en
"set spell
set nospell
set hidden
set history=1000

" Folding
set foldenable
set foldlevelstart=10
set foldnestmax=10
"set foldmethod=indent
set foldmethod=syntax


" Make vim save and load the folding of the document each time it loads
" also places the cursor in the last place that it was left.
au BufWinLeave * mkview
au BufWinEnter * silent loadview

" Automatically remove all trailing spaces
au BufWritePre * :%s/\s\+$//e

" Extend % to match not only braces
runtime macros/matchit.vim

" longer that 120 chars is an error
match ErrorMsg '\%>120v.\+'
match ErrorMsg '\s\+$'

" KEYS """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let mapleader = "\<SPACE>"

" Move between buffers
nmap <S-left> :bprev<CR>
nmap <S-right> :bnext<CR>
" window resizing with plus/minus keys
if bufwinnr(1)
    map + <C-W>+
    map - <C-W>-
endif

" You'll be able to move selected block up/down in Visual block mode.
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

nmap <F3> :Autoformat<CR>
nmap <F4> :YcmCompleter GoTo<CR>
nmap <F5> :YcmCompleter FixIt<CR>
nmap <F6> :YcmCompleter GetType<CR>
nmap <F7> :NERDTreeToggle<CR>
nmap <F8> :TagbarToggle<CR>
nmap <F9> :Ack <cword> .<CR>
nmap <F10> :Ack "FIXME\|TODO" .<CR>

" space open/closes folds
nnoremap <space> za

" ---------- Python
py3 << EOF
import os
import sys
if 'VIRTUAL_ENV' in os.environ:
  project_base_dir = os.environ['VIRTUAL_ENV']
  activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
  execfile(activate_this, dict(__file__=activate_this))
EOF

let python_highlight_all=1
" PEP8 Code Style
au BufRead,BufNewFile *.py set textwidth=79 shiftwidth=4 tabstop=4 expandtab softtabstop=4 shiftround autoindent

" ---------- HTML
au BufRead,BufNewFile *.html set textwidth=120 tabstop=2

" ---------- Javascript
au BufNewFile,BufReadPost *.json set filetype=javascript

" ---------- C++
au BufNewFile,BufReadPost *.h set filetype=cpp

" ---------- lisp
au BufNewFile,BufReadPost .spacemacs set filetype=lisp

" ---------- Make
""" http://vim.wikia.com/wiki/Automatically_open_the_quickfix_window_on_:make
" Automatically open, but do not go to (if there are errors) the quickfix /
" location list window, or close it when is has become empty.
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow
