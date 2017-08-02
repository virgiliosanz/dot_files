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

" Decorations
Plug 'flazz/vim-colorschemes'
Plug 'bling/vim-airline'

" Ctrl+p Open search for files
Plug 'ctrlpvim/ctrlp.vim'

Plug 'vim-scripts/Smart-Tabs'
Plug 'jiangmiao/auto-pairs'

Plug 'majutsushi/tagbar'
nmap <F8> :TagbarToggle<CR>

Plug 'vim-scripts/TaskList.vim'
nmap <F9> :TaskList<CR>

"Plug 'scrooloose/syntastic'
Plug 'sheerun/vim-polyglot'

" compile with (see what version of python is vim compiled with)
" python3 install.py --clang-completer --gocode-completer --racer-completer --tern-completer
Plug 'Valloric/YouCompleteMe', { 'do': 'python3 install.py --clang-completer --gocode-completer --racer-completer --tern-completer' }
Plug 'rdnetto/YCM-Generator', { 'branch': 'stable'}

" Snippets
"Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

Plug 'chiel92/vim-autoformat'
nmap <F3> :Autoformat<CR>

" Python
Plug 'davidhalter/jedi-vim', { 'for': 'python' }

" Elixir
"Plug 'slashmili/alchemist.vim', { 'for': 'elixir' }

" Rust
"Plug 'rust-lang/rust.vim', {'for': 'rust' }
"let g:ycm_rust_src_path = '/opt/local/share/rust/src/'
"let g:rustfmt_autosave = 1
"let g:rust_recommended_style = 1

" golang
"Plug 'fatih/vim-go', { 'do': ':GoInstallBinaries' }

" editorconfig.org
Plug 'editorconfig/editorconfig-vim'

" Syntax checking
Plug 'vim-syntastic/syntastic'
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

call plug#end()

filetype plugin indent on

" ------------ global -------------------------------------------
set guifont=Monaco:h12
set t_Co=256
"colorscheme ir_black
"colorscheme Molokai
"colorscheme wombat256
"colorscheme zenburn
colorscheme Jellybeans
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

" Make vim save and load the folding of the document each time it loads
" also places the cursor in the last place that it was left.
au BufWinLeave * mkview
au BufWinEnter * silent loadview

"" Automatically remove all trailing spaces
au BufWritePre * :%s/\s\+$//e

"" Keys
"let mapleader = ","
let mapleader = " "

"" YouCompleteMe & UltSnippets
let g:ycm_confirm_extra_conf = 0
nnoremap <leader>jg :YcmCompleter GoTo<CR>
nnoremap <leader>jf :YcmCompleter FixIt<CR>
nnoremap <leader>jt :YcmCompleter GetType<CR>
nnoremap <leader>jd :YcmCompleter GetDoc<CR>

let g:UltiSnipsExpandTrigger = "<c-j>"

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

"" Redefine ficheros por extensión
au BufNewFile,BufReadPost *.json set filetype=javascript
au BufNewFile,BufReadPost *.h set filetype=cpp

" virtualenv support
py << EOF
import os
import sys
if 'VIRTUAL_ENV' in os.environ:
  project_base_dir = os.environ['VIRTUAL_ENV']
  activate_this = os.path.join(project_base_dir, 'bin/activate_this.py')
  execfile(activate_this, dict(__file__=activate_this))
EOF
let python_highlight_all=1

" ------------ Make -----------------------------------------
""" http://vim.wikia.com/wiki/Automatically_open_the_quickfix_window_on_:make
" Automatically open, but do not go to (if there are errors) the quickfix /
" location list window, or close it when is has become empty.
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow
