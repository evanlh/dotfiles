call pathogen#runtime_append_all_bundles()
call pathogen#helptags()

let leader = '\'
noremap <leader>t <Esc>:CtrlP<CR>
noremap <leader>r <Esc>:CtrlPMRU<CR>
noremap <leader>g <Esc>:Ack 
noremap <leader>d <Esc>:NERDTreeToggle<CR>

noremap <leader>s <Esc>:split<CR>
noremap <leader>v <Esc>:vsplit<CR>

set expandtab
set tabstop=4
set shiftwidth=4
set autoindent smartindent
syntax on
highlight BadWhitespace ctermbg=red guibg=red

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

set background=dark
let g:solarized_termtrans=1
let g:solarized_termcolors=256
let g:solarized_contrast="high"
let g:solarized_visibility="high"
colorscheme solarized

set backupdir=/tmp
set noswapfile

set statusline=%F%m%r%h%w\ 
set statusline+=%{fugitive#statusline()}\ 
set statusline+=\ [%l\/%L]\ 

