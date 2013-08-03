filetype off
set rtp+=$GOROOT/misc/vim
set rtp+=~/.vim/bundle/vundle/
set rtp+=~/.vim/bundle/powerline/powerline/bindings/vim
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'rfru/vim-acejump'
Bundle 'terryma/vim-multiple-cursors'
Bundle 'altercation/vim-colors-solarized'
Bundle 'scrooloose/syntastic'
Bundle 'kien/ctrlp.vim'
Bundle 'rfru/ctrlp-dwim'
Bundle 'rfru/vim-gocode'
Bundle 'Raimondi/delimitMate'
Bundle 'Lokaltog/powerline'
Bundle 'pangloss/vim-javascript'
Bundle 'mileszs/ack.vim'
Bundle 'Valloric/MatchTagAlways'
Bundle 'HTML-AutoCloseTag'
Bundle 'majutsushi/tagbar'
Bundle 'Valloric/YouCompleteMe'

filetype plugin indent on
syntax on

set hidden        " allow hidden buffers
set backspace=indent,eol,start  "backspace thru anything"
set fileformats+=mac
set wildmenu      " show completion menu
set showmatch     " set show matching parenthesis
set title         " Let vim change title

set autoindent
set tabstop=2     " tab is how wide to display
set softtabstop=2 " tab counts as how may
set shiftwidth=2  " number of spaces to use for autoindenting
set shiftround    " use multiple of shiftwidth when indenting with '<' and '>'
set expandtab     " convert tabs to spaces
set smarttab      " insert tabs on the start of a line according to shiftwidth

set ignorecase    " ignore case when searching
set smartcase     " ignore case if search pattern is all lowercase, case-sensitive otherwise
set hlsearch      " highlight search terms
set incsearch     " show search matches as you type

set autochdir     " change cwd to open file

set laststatus=2  " Always display the statusline in all windows
set noshowmode    " Disable showing mode, since Powerline already displays
set display+=lastline " Show the last line instead of cutting it off with @

set autoread      " Reload any changed buffers
set cursorline    " Highlight cursor line
set pastetoggle=<Leader>p

set undofile
set undodir=/tmp

function! MyAck()
  let pattern = input('Pattern: ')
  if pattern != ''
    silent exe "Ack ".pattern
  endif
endfunction
nmap <S-f> :call MyAck()<Cr>

nnoremap f :call EasyMotion(0)<Cr>

nmap <Tab> :b#<Cr>
nmap , :CtrlPDwim<Cr>
nnoremap <silent> t :TagbarToggle<CR>

let g:multi_cursor_use_default_mapping=0
let g:multi_cursor_next_key='m'
let g:multi_cursor_quit_key='<Esc>'

let g:ctrlp_user_command = ''
let g:ctrlp_working_path_mode = 'c'
let g:ctrlp_match_window = 'bottom,order:ttb,min:1,max:20'
let g:ctrlp_use_caching = 0
let g:ctrlp_extensions = ['dwim']
let g:ctrlp_abbrev = {'abbrevs': [{'pattern': '\s', 'expanded': '.*'}]}
let g:ctrlp_regexp = 1

let g:tagbar_autofocus = 1
let g:tagbar_foldlevel = 2

set background=light
colorscheme solarized

source $VIMRUNTIME/macros/matchit.vim

" Automatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %

" Relies on nsf/gocode, $GOROOT, $GOPATH
autocmd FileType go set omnifunc=gocomplete#Complete
autocmd FileType go autocmd BufWritePre <buffer> Fmt
autocmd FileType go setlocal noexpandtab

let g:netrw_list_hide = ".git,.sass-cache,.jpg,.png,.svg"

highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/
autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
autocmd InsertLeave * match ExtraWhitespace /\s\+$/
autocmd BufWinLeave * call clearmatches()

hi MatchParen ctermbg=gray guibg=gray

let g:EclimCompletionMethod = 'omnifunc'
map <Leader>o :JavaImportOrganize<Cr>

" Jumps to last known position in file after loading.
" :help last-position-jump
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif


""""""""""""""
" GOOGLE STUFF
""""""""""""""
"source /usr/share/vim/google/google.vim
"source /usr/share/vim/google/gtags.vim
"Glug blaze do/mappings=<Leader>b
"Glug g4
"autocmd FileType javascript set omnifunc=JSDRichCompletions
"autocmd FileType jslayout,html set textwidth=0
"function! Glaze()
""  let currentDir = expand('%:p:h')
"  silent exe "!glaze ".currentDir
""  redraw!
"endfunction
"autocmd FileType go autocmd BufWritePost * if (match(expand('%:p'),'^/google/src/cloud') != -1) | call Glaze() | endif
