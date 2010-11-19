set nocompatible
set backspace=indent,eol,start
set nobackup		 " do not keep a backup file, use versions instead
set history=50		 " keep 50 lines of command line history
set ruler		 " show the cursor position all the time
set showcmd		 " display incomplete commands
set incsearch		 " do incremental searching

" Folding
set foldcolumn=3         " 2 lines of column for fold showing, always
set foldmethod=syntax
set foldlevelstart=99

" Otros
set titlestring=%f title " Display filename in terminal window
set nohlsearch           " nohighlight searches
set ignorecase           " make searches case-insensitive, unless they contain upper-case letters:
set smartcase
set incsearch            " show the `best match so far' as search strings are typed:
set enc=utf-8            " UTF-8 Default encoding
set tabstop=8 softtabstop=4 shiftwidth=4 expandtab
set autoindent           
set smartindent
set hidden               " You can change buffer without saving
set noerrorbells         " Stop noise
set visualbell
set showmatch            " Show matching brackets
set cursorline           " highlight current line
set laststatus=2         " always show the status line
set lazyredraw           " do not redraw while running macros
set linespace=0          " don't insert any extra pixel lines
"set number

" Don't use Ex mode, use Q for formatting
map Q gq
if has("gui_running")
   set lines=43 columns=120 " perfect size for me
   set mousehide " hide the mouse cursor when typing
endif

filetype plugin on
filetype plugin indent on
helptags ~/.vim/doc

" Colors
" **********************************************************************
set t_Co=256 " 256 colors
set background=dark 
syntax on " syntax highlighting
"colorscheme ir_black
"colorscheme gardener
colorscheme railscasts

" ruby
autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1

" NERDTree
"autocmd VimEnter * NERDTree
"autocmd VimEnter * wincmd p

" BufExplorer
let g:bufExplorerShowDirectories=0   " Don't show directories.

" keys
map <F2> :NERDTreeToggle<CR>
nnoremap <silent> <F3> :TlistToggle<CR>


" Correct some spelling mistakes
ia teh      the
ia htis     this
ia tihs     this
ia funciton function
ia fucntion function
ia funtion  function
ia retunr   return
ia reutrn   return
ia sefl     self
ia eslf     self
