set nocompatible
filetype off

let mapleader="\<SPACE>"

map <F9> :CMake<CR> :wa<CR> :make <CR>
imap <F9> <ESC> :CMake<CR> :wa<CR> :make <CR>

nmap <F2> :wa<CR> <Plug>(altr-forward)
nmap <F3> :wa<CR> <Plug>(altr-back)

let g:ycm_server_keep_logiles = 1
let g:ycm_server_log_level = 'debug'

" set colorcolumn=80
set laststatus=2
set updatetime=500
set completeopt=menuone,preview,noinsert

set showmatch
set number
set formatoptions+=o
set expandtab
set tabstop=4
set shiftwidth=4
set nojoinspaces

set ignorecase
set smartcase

set splitbelow
set splitright
set nostartofline

" Tell Vim which characters to show for expanded TABs,
" trailing whitespace, and end-of-ines.
if &listchars ==# 'eol:$'
    set listchars=tab:>\ , trail:-,extends:>,precedes:<,nbsp:+
endif
set list

highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
match ExtraWhitespace /\s\+$\|\t/

highlight Normal ctermbg=Black
highlight NonText ctermbg=Black

set path+=**
set wildmenu
set showcmd
set autoread

" Triger `autoread` when files changes on disk
autocmd FocusGained,BufEnter,CursorHold,CursorHoldI * if mode() != 'c' | checktime | endif

" Notification after file change
autocmd FileChangedShellPost *
  \ echohl WarningMsg | echo "File changed on disk. Buffer reloaded." | echohl None

call plug#begin('~/.vim/plugged')

    Plug 'ctrlpvim/ctrlp.vim'
    Plug 'vim-airline/vim-airline'
    Plug 'tpope/vim-surround'
    Plug 'scrooloose/nerdcommenter'

    Plug 'Valloric/YouCompleteMe', {'do': './install.py --clang-completer' }
    Plug 'octol/vim-cpp-enhanced-highlight'

    Plug 'vhdirk/vim-cmake'
    Plug 'kana/vim-altr'

    Plug 'jiangmiao/auto-pairs'

    Plug 'jreybert/vimagit'

    Plug 'vim-airline/vim-airline-themes'
    Plug 'mhartington/oceanic-next'

    Plug 'ryanoasis/vim-devicons'

call plug#end()

set encoding=utf-8

call altr#remove_all()
call altr#define('%/source/%.cpp', '%/include/zodiac/%.h')

"let g:airline_powerline_fonts = 1
"let g:airline_theme= 'oceanicnext'

if (has("termguicolors"))
    set termguicolors
endif

set cindent

syntax enable
colorscheme OceanicNext
