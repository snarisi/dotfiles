" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
" call vundle#begin('~/some/path/here')

Plugin 'VundleVim/Vundle.vim'

Plugin 'editorconfig/editorconfig-vim'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/nerdcommenter'
Plugin 'vim-scripts/indentpython'
Plugin 'ervandew/supertab'
Plugin 'vim-syntastic/syntastic'
Plugin 'terryma/vim-multiple-cursors'
Plugin 'jiangmiao/auto-pairs'
Plugin 'pangloss/vim-javascript'
Plugin 'rking/ag.vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'tpope/vim-fugitive'
Plugin 'sjl/vitality.vim'

" All of your Plugins must be added before the following line
call vundle#end()            " required

syntax on
filetype plugin indent on
colorscheme desert
set number
set tabstop=4
set shiftwidth=4
set softtabstop=4
set fileformat=unix
set autoread
au FocusGained,BufEnter * :checktime

" keybindings
noremap <silent> \ :NERDTreeToggle<CR>
noremap <silent> 09 :tabp<CR>
noremap <silent> 90 :tabn<CR>
noremap <S-Up> :wincmd k<CR>
noremap <S-Down> :wincmd j<CR>
noremap <S-Left> :wincmd h<CR>
noremap <S-Right> :wincmd l<CR>
noremap <C-k> :wincmd k<CR>
noremap <C-j> :wincmd j<CR>
noremap <C-h> :wincmd h<CR>
noremap <C-l> :wincmd l<CR>
let mapleader=","

" pythn indentation
au BufNewFile,BufRead *.py
	   \ set tabstop=4 |
	   \ set softtabstop=4 |
	   \ set shiftwidth=4 |
	   "\ set colorcolumn=80 |
	   \ set expandtab |
	   \ set autoindent |
	   \ set fileformat=unix |

" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_always_populate_loc_list = 1
let g:syntax_auto_loc_list = 1
let g:syntax_check_on_open = 0
let g:syntastic_check_on_wq = 1

" NERDTree 
let NERDTreeShowHidden=1

" Ag
let g:ag_working_path_mode="r"

" pathogen
" execute pathogen#infect()
                                                        "
