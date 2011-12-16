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
colorscheme gardener
"colorscheme railscasts

" Snipets
let g:snips_author="Virgilio Sanz <virgilio.sanz@cocinario.es>"

" NERDTree
"autocmd VimEnter * NERDTree
"autocmd VimEnter * wincmd p

" BufExplorer
let g:bufExplorerShowDirectories=0   " Don't show directories.

" ruby
autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
autocmd FileType ruby,eruby let g:rubycomplete_rails = 1
autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1

autocmd FileType c,cpp        setlocal foldmethod=syntax foldnestmax=2 cinoptions=(0,h0
autocmd FileType erlang,ocaml setlocal foldmethod=indent expandtab tabstop=4 shiftwidth=4
autocmd FileType python       setlocal foldmethod=indent
autocmd BufEnter *.txt,README,TODO,*.markdown,*.md if &filetype == '' | setlocal filetype=txt | endif
autocmd FileType txt,tex,mail,asciidoc setlocal textwidth=72 colorcolumn=+1 spell

" Clang Complete plugin:
let clang_use_library     = 1
let clang_complete_auto   = 0
let clang_complete_macros = 1
let clang_complete_copen  = 1
autocmd FileType c,cpp setlocal completeopt=menuone
autocmd FileType c,cpp highlight clear SpellBad   | highlight SpellBad ctermfg=white ctermbg=red
autocmd FileType c,cpp highlight clear SpellLocal | highlight SpellLocal ctermfg=white ctermbg=blue
autocmd FileType c,cpp map <buffer> <silent> <Leader>e :call g:ClangUpdateQuickFix()<Enter>

" Vimerl plugin:
let erlang_folding     = 1
let erlang_show_errors = 0
let erlang_man_path    = '/usr/local/lib/erlang/man'
let erlang_skel_header = {'author': 'Ricardo Catalinas Jiménez <jimenezrick@gmail.com>',
                       \  'owner' : 'Ricardo Catalinas Jiménez'}

" Syntastic plugin:
let syntastic_enable_signs       = 1
let syntastic_auto_loc_list      = 1
let syntastic_disabled_filetypes = ['c', 'cpp', 'erlang', 'ocaml', 'python', 'tex', 'sh',
                                 \  'cuda', 'css', 'html', 'xhtml', 'xml', 'xslt']

match Todo /TODO\|FIXME\|XXX\|FUCKME/

" keys
map <silent> <F1>  :write<Enter>
map <silent> <F2>  :NERDTreeToggle<Enter>
"map <silent> <F3>  :nohlsearch<Enter>
nnoremap <silent> <F3> :TlistToggle<CR>
map <silent> <F4>  :TagbarToggle<Enter>
map <silent> <F5>  :make<Enter>
map <silent> <F6>  :shell<Enter>
map <silent> <F7>  :if <SID>ToggleAutoHighlight()<Bar>set hlsearch<Bar>else<Bar>nohlsearch<Bar>endif<Enter>
map <silent> <F8>  :vimgrep /TODO\\|FIXME\\|XXX\\|FUCKME/ %<Enter>:copen<Enter>
map <silent> <F9>  :checktime<Enter>
map <silent> <F11> :w!<Enter>:!aspell check %<Enter>:w %<Enter>
map <silent> <F12> :SpellThis<Enter>

" Use Tabular plugin to align variable assignments
map <silent> <Leader>t=       :Tabularize /^[^=]*\zs=<Enter>
" Use Tabular plugin to align variable declarations
map <silent> <Leader>t<Space> :Tabularize /^\s*\S*\zs\(\s\*\\|\s&\\|\s\)/l0r0<Enter>

" Adds spaces around current block of lines
map <silent> <Leader><Space> :call <SID>AddSpaces()<Enter>
" Removes spaces around current block of lines
map <silent> <Leader><BS>    :call <SID>RemoveSpaces()<Enter>
" Collapses current block of blank lines to one
map <silent> <Leader><Del>   :call <SID>CollapseSpaces()<Enter>

function s:AddSpaces() range
        let separation = 2
        let blanks     = repeat([''], separation)
        call append(a:lastline, blanks)
        call append(a:firstline - 1, blanks)
endfunction

function s:RemoveSpaces()
        if getline('.') == ''
                let fromline = prevnonblank(line('.')) + 1
                let toline   = nextnonblank(line('.')) - 1
                call s:DeleteLines(fromline, toline, 0)
                return
        endif

        let toline = search('^$', 'bn')
        if toline != 0
                let fromline = prevnonblank(toline) + 1
                call s:DeleteLines(fromline, toline)
        endif

        let fromline = search('^$', 'n')
        if fromline != 0
                let toline = nextnonblank(fromline) - 1
                call s:DeleteLines(fromline, toline)
        endif
endfunction

function s:CollapseSpaces()
        if getline('.') != ''
                return
        endif

        if line('.') > 1 && getline(line('.') - 1) == ''
                let toline   = line('.') - 1
                let fromline = prevnonblank(toline) + 1
                call s:DeleteLines(fromline, toline)
        endif

        if line('.') < line('$') && getline(line('.') + 1) == ''
                let fromline = line('.') + 1
                let toline   = nextnonblank(fromline) - 1
                call s:DeleteLines(fromline, toline)
        endif
endfunction

function s:DeleteLines(fromline, toline, ...)
        let toline = a:toline < 1 ? line('$') : a:toline
        silent execute a:fromline . ',' . toline . 'delete'
        if a:0 == 0 || a:0 == 1 && a:1
                normal ``
        endif
endfunction

function s:ToggleAutoHighlight()
        if exists('#auto_highlight')
                autocmd! auto_highlight
                augroup! auto_highlight
                augroup END
                set updatetime&
                return 0
        else
                augroup auto_highlight
                        autocmd!
                        autocmd CursorHold * let @/ = '\V\<' . escape(expand('<cword>'), '\') . '\>'
                augroup END
                set updatetime=500
                return 1
        endif
endfunction
