" Iniciamos el entorno
filetype off
call pathogen#runtime_append_all_bundles()
filetype plugin indent on
set nocompatible

" Configuración básica
set encoding=utf-8
set autoindent
set showmode
set showcmd
set hidden
set visualbell
set cursorline
set ttyfast
set ruler
set backspace=indent,eol,start
set nonumber
"set norelativenumber
set laststatus=2
set history=1000
"set undofile
"set undoreload=10000
set cpoptions+=J
"set list  " Marca el final de línea: no me gusta
set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
set lazyredraw
set matchtime=3
set showbreak=↪
set splitbelow
set splitright
set fillchars=diff:⣿
set ttimeout
set notimeout
set nottimeout
set autowrite
set shiftround
set autoread
set title
set dictionary=/usr/share/dict/words

" Wildmenu - Como pude vivir sin esto...
set wildmenu
set wildmode=list:longest,full
set wildignore+=.hg,.git,.svn,.cvs               " Version control
set wildignore+=*.aux,*.out,*.toc                " LaTeX intermediate files
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg   " binary images
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest " compiled object files
set wildignore+=*.a,*.so                         " compiled object files
set wildignore+=*.spl                            " compiled spelling word lists
set wildignore+=*.sw?                            " Vim swap files
set wildignore+=*.DS_Store                       " OSX bullshit
set wildignore+=*.luac                           " Lua byte code
set wildignore+=migrations                       " Django migrations
set wildignore+=*.pyc                            " Python byte code
set wildignore+=*.beam                           " Erlang byte code

" Buffer tabs
"set showtabline=2
"set tabpagemax=20

" Make Vim able to edit crontab files again.
set backupskip=/tmp/*,/var/tmp/*"

" Save when losing focus
au FocusLost * :wa

" Resize splits when the window is resized
au VimResized * exe "normal! \<c-w>="

" Tabs, spaces, wrapping
set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
syntax match Tab /\t/
hi Tab gui=underline guifg=blue ctermbg=blue
set wrap
set textwidth=80
set formatoptions=qrn1
"set colorcolumn=+1

" Backups
"set undodir=~/.vim/tmp/undo/
set backupdir=~/.vim/tmp/backup/
set directory=~/.vim/tmp/swap/
set backup
set noswapfile

" Cambiamos la tecla leader a una coma
let mapleader = ","
let maplocalleader = "\\"

" Color scheme
"let c_comment_strings=1
set t_Co=256
syn on
"colorscheme leo
"colorscheme desert256
"colorscheme railscasts
"colorscheme gardener
colorscheme herald

" Shortcuts
nmap <leader>w :w!<cr>
"nmap <leader>b :b <tab>
"nmap <leader>e :e <tab>

" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" START: Status line -----------------------------------------
set statusline=%f    " Path.
set statusline+=%m   " Modified flag.
set statusline+=%r   " Readonly flag.
set statusline+=%w   " Preview window flag.
set statusline+=\    " Space.
set statusline+=%#redbar#                " Highlight the following as a warning.
set statusline+=%{SyntasticStatuslineFlag()} " Syntastic errors.
set statusline+=%*                           " Reset highlighting.
set statusline+=%=   " Right align.
" File format, encoding and type.  Ex: "(unix/utf-8/python)"
set statusline+=(
set statusline+=%{&ff}                        " Format (unix/DOS).
set statusline+=/
set statusline+=%{strlen(&fenc)?&fenc:&enc}   " Encoding (utf-8).
set statusline+=/
set statusline+=%{&ft}                        " Type (python).
set statusline+=)
" Line and column position and counts.
set statusline+=\ %l\/%L,%c
" END: Status line -----------------------------------------

" Search & Movement
set ignorecase
set smartcase
set incsearch
set showmatch
set hlsearch
set gdefault

" Folding
set foldlevelstart=999 " Me gusta ver el contenido de las funciones, comentarios....
set foldmethod=syntax

" Space to toggle folds.
nnoremap <Space> za
vnoremap <Space> za

" Make zO recursively open whatever top level fold we're in, 
" no matter where the cursor happens to be.
nnoremap zO zCzO


" KEYS -----------------
" Formatting, TextMate-style
nnoremap Q gqip

" Better Completion
set completeopt=longest,menuone,preview

" PLUGINS -----------------------------------------------

" c.vim         -----------------------------------------

" delimitMate   -----------------------------------------

" matchit       -----------------------------------------
runtime macros/matchit.vim
map <tab> %

" mru           -----------------------------------------

" nerdcommenter -----------------------------------------
" <leader>cc <leader>cn - Comenta línea o selección
" <leader>cu            - Descomenta línea o selección

" NERDTRee    -------------------------------------------
noremap  <F2> :NERDTreeToggle<cr>
inoremap <F2> <esc>:NERDTreeToggle<cr>

au Filetype nerdtree setlocal nolist
let NERDTreeHighlightCursorline=1
let NERDTreeMinimalUI = 1
let NERDTreeDirArrows = 1

" PIV         -------------------------------------------
" Simply hit K (shift+k) on any function to see full documentation

" QuickBuf    -------------------------------------------
" No funciona ni por defecto ni esta:
" let g:qb_hotkey = "<F3>" 

" snipMate    -------------------------------------------
" @see ~/.vim/bundle/snipmate.vim/snippets

" sparkup     -------------------------------------------
let g:sparkup = '$HOME/.vim/bundle/sparkup/ftplugin/html/sparkup.py'
" Este plugin no me funciona... :'(

" Syntastic -------------------------------------------
" Lista de syntax checkers en
" ~/.vim/bundle/syntastic/syntax_checkers
let g:syntastic_enable_signs = 1
let g:syntastic_disabled_filetypes = ['html']
let g:syntastic_stl_format = '[%E{%e Errors}%B{, }%W{%w Warnings}]'
let g:syntastic_jsl_conf = '$HOME/.vim/jsl.conf'
let g:syntastic_check_on_open=1
let g:syntastic_auto_jump=1
let g:syntastic_auto_loc_list=2
let g:syntastic_mode_map = { 'mode': 'active',
    \ 'active_filetypes': ['ruby', 'php', 'cpp', 'css', 'erlang', 'html', 'javascript', 'json', 'python', 'tex', 'xml'],
    \ 'passive_filetypes': ['puppet'] }

" Surround  -------------------------------------------

" Vimerl    -------------------------------------------
let g:erlang_skel_header = { "author": "Virgilio Sanz", "owner" : "" } 
let g:erlang_show_errors = 1

" TagBar    -------------------------------------------
nnoremap <silent> <F3> :TagbarToggle<CR>

" UTL       -------------------------------------------
let utl_opt_verbose=1

" vimGTD      -------------------------------------------
map ,gtd :!gtd %<C-M>:e<C-M><C-M>
