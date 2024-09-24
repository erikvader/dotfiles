" Don't make vim vi compatible. This option must be at the very top
set nocompatible

" --- whitespace ---

" Copy indent from previous line
set autoindent

" Insert spaces instead of a tab character
set expandtab

" How wide a tab character is when displayed
set tabstop=4

" How many spaces to insert when a tab is inserted. Negative values uses whatever 'shiftwidth' is set to.
set softtabstop=-1

" How many spaces to use when shifting with >> and <<. When zero 'tabstop' is used
set shiftwidth=0

" Uses spaces for a tab in more places???
set smarttab

" Round indents to multiples of 'shiftwidth'
set shiftround

" Show tabs as another character
set listchars=tab:>-
set list

" --- System ---

" Enables filetype detection, plugins for different file types and indentation rules
filetype plugin indent on

" Use UTF-8
set encoding=utf-8

" Increase command history
set history=1000

" Decreases keycode delay from the terminal, they are fast enough these days
" https://unix.stackexchange.com/a/608179
set ttimeout
set ttimeoutlen=10

" --- GUI/TUI ---

" Do not ring the bell
set noerrorbells

" Always show status lines
set laststatus=2

" Visually wrap long lines beyond the window
set wrap

" Show line and column numbers
set ruler

" Relative line numbers
set number relativenumber

" Set the window title
set title

" Change cursor shapes in insert mode
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

" --- Syntax highlighting ---

" Enable syntax highlighting
syntax on

" Use colors that look good on dark background
set background=dark

" Change colortheme. Doesn't work well on a transparent terminal
"colorscheme desert

" --- Brackets ---

" <> counts as a paranthesis pair
set matchpairs+=<:>

" Show matching bracket briefly
set showmatch

" --- Search ---

" Highlight previous search
set hlsearch

" Highlight the current search pattern while typing
set incsearch

" Ignore case when searching
set ignorecase

" Be case sensitive when searching with at least one capital letter
set smartcase

" Temporarily diable hlsearch
nnoremap <BS> :noh<CR>
" nnoremap <BS> :let @/=''<CR>

" --- Formatting ---
" Remove comment leaders when joining lines
set formatoptions+=j

" --- Editing ---

" More useful binding for U
nnoremap U :redo<CR>

" The cursor can move on non-existent characters
set virtualedit=block

" Removes underscore from being a word character
set iskeyword-=_

