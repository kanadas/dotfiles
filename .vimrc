" ---------------------- USABILITY CONFIGURATION ----------------------
" make vim try to detect file types and load plugins for them
filetype on
filetype plugin on
filetype indent on
syntax on

" don't make vim compatible with vi
set nocompatible
set number relativenumber
set wildmode=longest,list,full
set nosplitbelow splitright

" cursor shape
let &t_SI = "\<Esc>[6 q"
let &t_SR = "\<Esc>[4 q"
let &t_EI = "\<Esc>[2 q"

augroup project
    autocmd!
    autocmd BufRead,BufNewFile *.h,*.c set filetype=c
    autocmd BufRead,BufNewFile *.pl set filetype=prolog
    autocmd BufRead,BufNewFile *.sql set filetype=plsql
    autocmd BufRead,BufNewFile *.m,*.oct set filetype=octave
    autocmd BufRead,BufNewFile *.asm set filetype=nasm
    autocmd BufRead,BufNewFile *.S set filetype=asm
augroup END

"Default use a tab width of 4 space chars.
set tabstop=4       " The width of a TAB is set to 4.
                    " Still it is a \t. It is just that
                    " Vim will interpret it to be having
                    " a width of 4.
set shiftwidth=4    " Indents will have a width of 4.
set softtabstop=4   " Sets the number of columns for a TAB.
set expandtab       " Expand TABs to spaces.
set shiftround      "Round indent to nearest shiftwidth multiple
set encoding=utf-8
set fenc=utf-8
"set mouse=a
set autoindent
autocmd FileType make set tabstop=8 shiftwidth=8 softtabstop=0 noexpandtab
autocmd FileType javascript set tabstop=4 shiftwidth=4 softtabstop=4
autocmd FileType haskell set tabstop=8 softtabstop=4 shiftwidth=4
autocmd FileType c set tabstop=8 shiftwidth=8 softtabstop=8 noexpandtab
autocmd FileType sql set shiftwidth=2 softtabstop=2 expandtab "encoding=latin2 "fenc=iso8859_2
autocmd FileType plsql set shiftwidth=2 softtabstop=2 expandtab "encoding=iso8859_2 fenc=iso8859_2
autocmd FileType xml set shiftwidth=2 softtabstop=2 expandtab "encoding=iso8859_2 fenc=iso8859_2

" enable matchit plugin which ships with vim and greatly enhances '%'
runtime macros/matchit.vim

" by default, in insert mode backspace won't delete over line breaks, or
" automatically-inserted indentation, let's change that
set backspace=indent,eol,start

" dont't unload buffers when they are abandoned, instead stay in the
" background
set hidden

" set unix line endings
set fileformat=unix
" when reading files try unix line endings then dos, also use unix for new
" buffers
set fileformats=unix,dos

" save up to 100 marks, enable capital marks
set viminfo='100,f1

" screen will not be redrawn while running macros, registers or other
" non-typed comments
set lazyredraw

" Starting with Vim 7, the filetype of empty .tex files defaults to
" 'plaintex' instead of 'tex', which results in vim-latex not being loaded.
" The following changes the default filetype back to 'tex':
let g:tex_flavor='latex'

" map pl spellchecking to <F6>
map <F6> :set spell! spelllang=pl<CR>
map <F7> :set spell! spelllang=en<CR>

" Ctags shortcuts: new tab and vsplit
map <C-W><C-\> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>
map <C-W><C-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>

vnoremap <C-c> "*y :let @+=@*<CR>
map <C-p> "+P

" move viminfo files
set viminfo+=n~/.vim/viminfo

"color 81 column:
"set colorcolumn=81

"highlight ColorColumn ctermbg=Black ctermfg=DarkRed
" Highlight trailing spaces
" http://vim.wikia.com/wiki/Highlight_unwanted_spaces
"highlight ExtraWhitespace ctermbg=red guibg=red
"match ExtraWhitespace /\s\+$/
"autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
"autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
"autocmd InsertLeave * match ExtraWhitespace /\s\+$/
"autocmd BufWinLeave * call clearmatches()

"remove all trailing spaces
:nnoremap <silent> <F5> :let _s=@/ <Bar> :%s/\s\+$//e <Bar> :let @/=_s <Bar> :nohl <Bar> :unlet _s <CR>

"remove all tex build files when leaving vim
autocmd VimLeave *.tex :VimtexClean

function! RemoveTrailingSpaces()
   let save_pos = getpos(".")
    %s/\s\+$//e
   call setpos(".", save_pos)
endfunction

"remove all trailing spaces on save
autocmd BufWritePre * call RemoveTrailingSpaces()


" ---------------------- PLUGIN CONFIGURATION ----------------------
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

" plugin definitions:
Plugin 'Valloric/YouCompleteMe'
Plugin 'scrooloose/nerdtree'
Plugin 'itchyny/lightline.vim'
Plugin 'w0rp/ale'
Plugin 'lervag/vimtex'
Plugin 'xuhdev/vim-latex-live-preview'
Plugin 'ctrlpvim/ctrlp.vim'
"Plugin 'cosminadrianpopescu/vim-sql-workbench'
Plugin 'bitc/vim-hdevtools'
Plugin 'joe-skb7/cscope-maps'
"Plugin 'vim-syntastic/syntastic'
" end plugin definitions
call vundle#end()            " required
filetype plugin indent on    " required

"YCM congiguration
let g:ycm_global_ycm_extra_conf = '~/.vim/global_ycm_extra_conf.py'
let g:ycm_confirm_extra_conf = 0
let g:ycm_autoclose_preview_window_after_completion = 1
" vimtex configuration
" default compiler is latexmk
"let g:vimtex_compiler_method = 'latexrun'
if !exists('g:ycm_semantic_triggers')
	let g:ycm_semantic_triggers = {}
endif
let g:ycm_semantic_triggers.tex = g:vimtex#re#youcompleteme
"Blacklisting c, because ale works better for kernel code
"let g:ycm_filetype_blacklist = {'c': 1}

"Ale configuration
" Enable completion where available.
" This setting must be set before Ale is loaded.
let g:ale_completion_enabled = 1
nmap <C-G> :ALEGoToDefinition
nmap <C-F> :ALEFindReferences
nmap <C-H> :ALEHover
nmap <C-I> :ALEDetail<CR>
nmap <silent> <C-k> <Plug>(ale_previous_wrap)
nmap <silent> <C-j> <Plug>(ale_next_wrap)
" turns of every linter except hdevtools and hides libiserv package
"Blacklisting c, c++, java because ycm works better
let g:ale_linters = {
\   'haskell': ['hdevtools'],
\   'c': [],
\   'c++': [],
\   'java': []
\}
"let g:ale_linters_ignore = ['clangd']
let g:ale_nasm_nasm_options = '-f elf64'
let g:ale_c_gcc_options = '-std=c11 -Wall -Wextra'
" Temporary - some obsolete error in 1st JPP task --
"let g:ale_haskell_hdevtools_options = '-g -isrc -g -Wall -g -hide-package -g libiserv'
let g:ale_fixers = {
\   'c': ['remove_trailing_lines', 'trim_whitespace']
\}
" after 20 keystrokes quickfix window will disappear
"let g:vimtex_quickfix_autoclose_after_keystrokes = 20

"Hdevtools bindings
au FileType haskell nnoremap <buffer> <F1> :HdevtoolsType<CR>
au FileType haskell nnoremap <buffer> <silent> <F2> :HdevtoolsInfo<CR>
au FileType haskell nnoremap <buffer> <silent> <F3> :HdevtoolsClear<CR>

"lightline configs
set laststatus=2 noshowmode

"Enables project-specific vim configs (and forbids them weird things)
set exrc secure

"------------------OLD CONFIGS-------------------------
" start NERDTree on start-up and focus active window
"autocmd VimEnter * NERDTree
"autocmd VimEnter * wincmd p
"autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Syntastic recommended configuration
"set statusline+=%#warningmsg#
"set statusline+=%{SyntasticStatuslineFlag()}
"set statusline+=%*
"let g:syntastic_always_populate_loc_list = 1
"let g:syntastic_auto_loc_list = 1
"let g:syntastic_check_on_open = 1
"let g:syntastic_check_on_wq = 0
" Diable syntastic for java, because i have YCM
"let g:syntastic_java_checkers = []
" Syntastic asm configuration
"let g:syntastic_nasm_checkers = ['nasm']
"let g:syntastic_nasm_nasm_args = ['-f elf64']

"CtrlP and SQLWorkbech config
"let g:ctrlp_map = '<c-p>'
"let g:ctrlp_cmd = 'CtrlP'
"let g:sw_exe = '/opt/SQLWorkbench/sqlwbconsole.sh'
"let g:ctrlp_extensions = ['sw_profiles']
"let g:sw_config_dir = '/home/tkanas/.sqlworkbench'


