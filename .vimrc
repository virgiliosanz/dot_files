set nocompatible

" Load vim-plug
if empty(glob("~/.vim/autoload/plug.vim"))
	execute '!mkdir -p ~/.vim/plugged'
	execute '!mkdir -p ~/.vim/autoload'
	execute '!curl -fLo ~/.vim/autoload/plug.vim https://raw.github.com/junegunn/vim-plug/master/plug.vim'
endif
call plug#begin('~/.vim/plugged')
Plug 'junegunn/vim-plug'

Plug 'flazz/vim-colorschemes'
Plug 'itchyny/lightline.vim'
Plug 'sheerun/vim-polyglot'

Plug 'vim-scripts/Smart-Tabs'
Plug 'jiangmiao/auto-pairs'

Plug 'editorconfig/editorconfig-vim'

Plug 'liuchengxu/vim-which-key'
Plug 'kien/ctrlp.vim'
let g:ctrlp_custom_ignore = '\v[\/]\.(git|hg|svn)$'
let g:ctrlp_working_path_mode = 'ra'

Plug 'ludovicchabant/vim-gutentags'
Plug 'majutsushi/tagbar'
let g:tagbar_foldlevel = 2

Plug 'chiel92/vim-autoformat'
au BufWrite * :Autoformat " Autoformat on save
au BufWritePre * :%s/\s\+$//e " Automatically remove all trailing spaces

"Plug 'rust-lang/rust.vim'
"let g:rustfmt_autosave = 1

" python3 install.py --clang-completer --rust-completer
"Plug 'ycm-core/YouCompleteMe'
"let g:ycm_min_num_of_chars_for_completion = 1
"let g:ycm_min_num_identifier_candidate_chars = 1
"let g:ycm_max_num_candidates = 20
"let g:ycm_auto_trigger = 1
"let g:ycm_confirm_extra_conf = 0
"let g:ycm_server_log_level = 'debug'
"let g:ycm_key_detailed_diagnostics = '<leader>d'

call plug#end()

filetype plugin indent on

" ------------ global -------------------------------------------
set guifont=Monaco:h12
set t_Co=256
"colorscheme ir_black
"colorscheme Molokai
"colorscheme wombat256
"colorscheme zenburn
"colorscheme jellybeans
"colorscheme colorsbox-material
"colorscheme badwolf
colorscheme solarized
let g:lightline = { 'colorscheme': 'solarized' }

syntax enable

set laststatus=2
set autoindent
set smartindent
set copyindent
set preserveindent
set cindent
set autoread
set nobackup
set nowritebackup
set noswapfile
set cursorline
set cursorcolumn
"set rnu
set nu
set encoding=utf8
set fileencoding=utf-8
set ffs=unix,mac,dos
set incsearch
set nohlsearch
set ignorecase
set smartcase
set lazyredraw
set linebreak
set showcmd
"set cmdheight=2
"set noshowmatch
"set noshowmode
set smarttab
set viminfo='100,f1
set visualbell

set path+=**
set wildmenu
set wildmode=list:longest
set wildignore=*.o,*.a,*.so,*.pyc,*.swp,.git/,*.class,*/target/*,*/build/*

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

set notimeout
set timeoutlen=300 " By default timeoutlen is 1000 ms
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
set signcolumn=yes

" Folding
"set foldenable
""set foldlevel=1
""set foldlevelstart=1
""set foldnestmax=1
"set foldmethod=syntax
"" space open/closes folds
"nnoremap <space> za

" Make vim save and load the folding of the document each time it loads
" also places the cursor in the last place that it was left.
au BufWinLeave * mkview
au BufWinEnter * silent loadview

" Extend % to match not only braces
runtime macros/matchit.vim

" longer that 120 chars is an error
match ErrorMsg '\%>120v.\+'
match ErrorMsg '\s\+$'

" Usar netrw en lugar de NERDTree
" https://shapeshed.com/vim-netrw/
let g:netrw_banner = 1
let g:netrw_liststyle = 3
"  *g:netrw_browse_split*    when browsing, <cr> will open the file by:
"                =0: re-using the same window  (default)
"                =1: horizontally splitting the window first
"                =2: vertically   splitting the window first
"                =3: open file in new tab
"                =4: act like "P" (ie. open previous window)
let g:netrw_browse_split = 0
let g:netrw_altv = 1
let g:netrw_winsize = 25
"augroup ProjectDrawer
"  autocmd!
"  autocmd VimEnter * :Vexplore
"augroup END


" KEYS """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:mapleader=' '
let g:maplocalleader=','
nnoremap <silent> <leader><leader> :<c-u>WhichKey '<Space>'<CR>
nnoremap <silent> <localleader> :<c-u>WhichKey ','<CR>

" Move between buffers
nmap <S-left> :bprev<CR>
nmap <S-right> :bnext<CR>

" window resizing with plus/minus keys
map + <C-W>+
map - <C-W>-

" Split window horizontal & vertical
"map _ <C-W>s
"map \| <C-W>v

" You'll be able to move selected block up/down in Visual block mode.
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

" Files
" =====
" Opens file under cursor in a new vertical split
nnoremap <leader>gf :vertical wincmd f<CR>
" Find word under cursor
nnoremap <leader>fw :execute "noautocmd vimgrep /" . expand("<cword>") . "/j **" <Bar> cw<CR>
" find TODO in project
nnoremap <leader>td :execute "noautocmd vimgrep /TODO/j **" <Bar> cw<CR>
" Find File in project
nnoremap <leader>ff :find
" Open netrw = File Explore
"nnoremap <leader>fe :Vexplore<CR>
nnoremap <leader>fe :Lexplore<CR>
" move to buffer
nnoremap <leader>bb :buffer

" Format
nnoremap <leader>af :Autoformat<CR>

"" YouCompleteMe
nnoremap <leader>yg :YcmCompleter GoTo<CR>
nnoremap <leader>yf :YcmCompleter FixIt<CR>
nnoremap <leader>yt :YcmCompleter GetType<CR>

" Tags
" -----
command! MakeTags !ctags -R .
"" Remap tags, not easy in a spanish keyboard
"" - Use ^] to jump to tag under cursor
nnoremap <leader>tj <C-]>
"" - Use ^t to jump back up the tag stack
nnoremap <leader>tb <C-T>
" Tag Selection: Jump to Definition
"nnoremap <leader>ts :tselect<CR>
nnoremap <leader>ts :ltag <c-r>=expand("<cword>")<cr><bar>lwindow<CR>
nnoremap <leader>tt :TagbarToggle<CR>

" Spell
" -----
nnoremap <leader>se :setlocal spell!  spelllang=en_uk<CR>
nnoremap <leader>ss :setlocal spell!  spelllang=es_es<CR>
nnoremap <leader>sn :setlocal nospell!<CR>

" ---------- Python
let python_highlight_all=1
" PEP8 Code Style
au BufRead,BufNewFile *.py set textwidth=79 shiftwidth=4 tabstop=4 expandtab softtabstop=4 shiftround autoindent

" ---------- HTML
au BufRead,BufNewFile *.html set textwidth=120 tabstop=2
au FileType html inoremap <leader>i <em></em><Space><++><Esc>Fet>i

" ---------- Javascript
au BufNewFile,BufReadPost *.json set filetype=javascript

" ---------- C/C++
au BufNewFile,BufReadPost *.h set filetype=cpp
au BufNewFile,BufReadPost *.ino set filetype=cpp
au BufNewFile,BufReadPost *.pde set filetype=cpp
au FileType cpp set keywordprg=cppman ts=4  " Hitting K search keyword
au FileType cpp set tags=tags,$HOME/tags
au FileType c   set tags=tags,$HOME/tags ts=8
let g:autoformat_autoindent = 0
let g:autoformat_retab = 0
let g:autoformat_remove_trailing_spaces = 0

" ---------- Rust
"  cargo install rusty-tags
autocmd BufRead *.rs :setlocal tags=./rusty-tags.vi;/
autocmd BufWritePost *.rs :silent! exec "!rusty-tags vi --quiet --start-dir=" . expand('%:p:h') . "&" | redraw!
autocmd BufRead,BufNewFile Cargo.toml,Cargo.lock,*.rs compiler cargo

" ---------- Make
""" http://vim.wikia.com/wiki/Automatically_open_the_quickfix_window_on_:make
" Automatically open, but do not go to (if there are errors) the quickfix /
" location list window, or close it when is has become empty.
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow
