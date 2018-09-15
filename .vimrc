" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
" alternatively, pass a path where Vundle should install plugins
" call vundle#begin('~/some/path/here')

Plugin 'VundleVim/Vundle.vim'

" Plugin 'editorconfig/editorconfig-vim'
" Plugin 'scrooloose/nerdtree'
" Plugin 'scrooloose/nerdcommenter'
" Plugin 'Vimjas/vim-python-pep8-indent'
" Plugin 'vim-syntastic/syntastic'
" Plugin 'w0rp/ale'
" Plugin 'terryma/vim-multiple-cursors'
" Plugin 'jiangmiao/auto-pairs'
" Plugin 'pangloss/vim-javascript'
" Plugin 'rking/ag.vim'
" Plugin 'airblade/vim-gitgutter'
" Plugin 'tpope/vim-fugitive'
" Plugin 'sjl/vitality.vim'
" Plugin 'christoomey/vim-tmux-navigator'
Plugin 'flazz/vim-colorschemes'
" Plugin 'felixhummel/setcolors.vim'
" Plugin 'SirVer/ultisnips'
" Plugin 'honza/vim-snippets'
" Plugin 'reedes/vim-pencil'
" Plugin 'junegunn/goyo.vim'
" Plugin 'vim-airline/vim-airline'
" Plugin 'tmux-plugins/vim-tmux-focus-events'
" Plugin 'ctrlpvim/ctrlp.vim'
" Plugin 'vim-airline/vim-airline-themes'
" Plugin 'mattn/emmet-vim'
" Plugin 'Valloric/YouCompleteMe'
" Plugin 'Glench/Vim-Jinja2-Syntax'
" Plugin 'JamshedVesuna/vim-markdown-preview'
" Plugin 'hdima/python-syntax'
" Plugin 'mxw/vim-jsx'
" Plugin 'tpope/vim-surround'
" Plugin 'andrep/vimacs'

" All of your Plugins must be added before the following line
call vundle#end()            " required

syntax on
filetype plugin indent on
set t_Co=256
autocmd VimEnter * colorscheme tropikos
set number
set tabstop=4
set shiftwidth=4
set softtabstop=4
set fileformat=unix
set autoread
set visualbell
au FocusGained,BufEnter * :checktime

" keybindings
" noremap <silent> \ :NERDTreeToggle<CR>
noremap <silent> 09 :tabp<CR>
noremap <silent> 90 :tabn<CR>
noremap <C-k> :wincmd k<CR>
noremap <C-j> :wincmd j<CR>
noremap <C-h> :wincmd h<CR>
noremap <C-l> :wincmd l<CR>
noremap <M-Up> :wincmd k<CR>
noremap <M-Down> :wincmd j<CR>
noremap <M-Left> :wincmd h<CR>
noremap <M-Right> :wincmd l<CR>
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

" airline
set laststatus=2
let g:airline#extensions#tabline#enabled = 1
let g:airline_theme='term'

" fugitive
" set statusline+=%{fugitive#statusline()}

" syntastic
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*
"let g:syntastic_always_populate_loc_list = 1
"let g:syntax_auto_loc_list = 1
"let g:syntax_check_on_open = 0
"let g:syntastic_check_on_wq = 1

" ALE
let g:ale_linters = {
\	'python': ['flake8'],
\	'javascript': ['eslint']
\}
" let g:ale_lint_on_text_changed=0
" let g:ale_lint_on_save=1
" let g:ale_statusline_format = ['⨉ %d', '⚠ %d', '⬥ ok']
" set statusline+=%{ALEGetStatusLine()}
" set statusline+=%*

" NERDTree 
" let NERDTreeShowHidden=1
" let NERDTreeIgnore=['\.swp$', '\.swo$']

" Ag
" let g:ag_working_path_mode="r"

" cursor
if exists('$TMUX')
	let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
	let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
	let &t_SI = "\<Esc>]50;CursorShape=0\x7"
	let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

" make backspace work again
set backspace=indent,eol,start

" airline
let g:airline#extensions#tabline#enabled = 1

" ultisnips
let g:UltiSnipsSnippetDirectories=["UltiSnips", "customsnips"]
" Trigger configuration. Do not use <tab> if you use 
" https://github.com/Valloric/YouCompleteMe.
" let g:UltiSnipsExpandTrigger="<c-k>"
" let g:UltiSnipsJumpForwardTrigger="<c-b>"
" let g:UltiSnipsJumpBackwardTrigger="<c-z>"
" let g:UltiSnipsListSnippets="<c-l>"

" If you want :UltiSnipsEdit to split your window.
" let g:UltiSnipsEditSplit="normal"

" pencil
" let g:pencil#textwidth = 120
" let g:pencil#wrapModeDefault = 'soft'
" augroup pencil
" 	autocmd!
" 	autocmd FileType markdown,mkd,text call pencil#init()
" augroup END

" youcompleteme
" nnoremap <leader>gt :YcmCompleter GoTo<cr>
" let g:ycm_autoclose_preview_window_after_completion = 1
" let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'

" markdown preview
" let vim_markdown_preview_hotkey='<C-m>'
" let vim_markdown_preview_browser='Google Chrome'
" let vim_markdown_preview_github=1

" python syntax
let python_version_2 = 1
let python_highlight_space_errors = 0
let python_highlight_all = 1

" jsx syntax
let g:jsx_ext_required = 0

" ctrlp
let g:ctrlp_custom_ignore = 'node_modules\|DS_Store\|git'

" emmet
let g:user_emmet_settings = {
\  'javascript' : {
\      'extends' : 'jsx',
\  },
\}
