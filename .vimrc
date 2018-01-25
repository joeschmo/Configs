set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
set rtp^=/usr/share/vim/vimfiles
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/syntastic'

Plugin 'vim-airline/vim-airline'
let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 0

Plugin 'reedes/vim-colors-pencil'
let g:airline_theme = 'pencil'

Plugin 'rust-lang/rust.vim'

Plugin 'blindFS/vim-taskwarrior'
Plugin 'jceb/vim-orgmode'
Plugin 'tpope/vim-speeddating'

Plugin 'vim-scripts/taglist.vim'
Plugin 'majutsushi/tagbar'
Plugin 'mattn/calendar-vim'
Plugin 'vim-scripts/SyntaxRange'
Plugin 'edkolev/tmuxline.vim'
Plugin 'benmills/vimux'

call vundle#end()
filetype plugin indent on

set number
set laststatus=2
syntax on
inoremap jk <Esc>
colorscheme pencil
set background=dark
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab
let g:python_host_prog = '/usr/bin/python2'
nnoremap <Space> :noh<CR>
nnoremap ,b :CtrlPBuffer<CR>
set nowrap
set colorcolumn=80
let mapleader = ","
let maplocalleader = "\\"
map <Leader>vp :VimuxPromptCommand<cr>

nmap <F8> :TagbarToggle<CR>
