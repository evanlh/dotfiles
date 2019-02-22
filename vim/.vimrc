execute pathogen#infect()

call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

let leader = '\'
noremap <leader>t <Esc>:CtrlP<CR>
noremap <leader>r <Esc>:CtrlPMRU<CR>
noremap <leader>e <Esc>:CtrlPClearCache<CR>
noremap <leader>g <Esc>:Ack 
noremap <leader>d <Esc>:NERDTreeToggle<CR>

noremap <leader>s <Esc>:split<CR>
noremap <leader>v <Esc>:vsplit<CR>

nnoremap ; :

set expandtab
set tabstop=4
set shiftwidth=4
set autoindent smartindent
syntax on
highlight BadWhitespace ctermbg=red guibg=red

autocmd FileType python map <buffer> <F4> :call Flake8()<CR>
let python_highlight_all=1

set mouse=a
set mousehide
set mousemodel=popup

command W w

set vb t_vb=

set number
highlight LineNr term=bold cterm=NONE ctermfg=DarkGrey ctermbg=NONE gui=NONE guifg=DarkGrey guibg=NONE

autocmd BufEnter * :syntax sync fromstart

filetype plugin on

if has("gui_running")
    set guioptions=egmrt
    set gfn=Andale\ Mono:h14
endif

"set background=dark
"let g:solarized_termtrans=1
"let g:solarized_termcolors=256
"let g:solarized_contrast="high"
"let g:solarized_visibility="high"
"colorscheme solarized

set backupdir=/tmp
set noswapfile

set statusline=%F%m%r%h%w\ 
set statusline+=%{fugitive#statusline()}\ 
set statusline+=\ [%l\/%L]\ 

set backupskip=/tmp/*,/private/tmp/*

syntax on
filetype plugin indent on

"Information on the following setting can be found with
":help set
set shiftwidth=4  "this is the level of autoindent, adjust to taste
set ruler
set backspace=indent,eol,start
set visualbell
nmap <F7> :tabp<CR>
nmap <F8> :tabn<CR>
set backspace=2
set t_kb=
set t_kD=[3~
" Uncomment below to make screen not flash on error
" set vb t_vb=""
