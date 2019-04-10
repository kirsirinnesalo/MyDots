"""""""""
"" vimrc

set encoding=utf-8

set history=1000

filetype plugin on
filetype indent on

set autoread

syntax on 

set background=dark
try
    colorscheme desert
catch
endtry

set guifont=Consolas:h10


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => files, backups, undo

try
    set undodir=~/vimfiles/undo//
    set undofile
catch
endtry
try
    set backupdir=~/vimfiles/backups//
catch
endtry
try
    set directory=~/vimfiles/tmp//
catch
endtry


"""""""""
"" keys

" define extra key combinations with mapleader
let mapleader=","
set wildcharm=<C-z>

" fast saving
nmap <leader>w :w!<cr>

" select colorscheme
nnoremap <leader>c :colorscheme <C-z><S-Tab>

" select buffer
nnoremap <leader>b :buffer <C-z><S-Tab>

" quickly open a scratch buffer
map <leader>q :e ~/scratch<cr>

" fast edit and reload vimrc
map <leader>e :e! ~/vimfiles/vimrc<cr>
autocmd! bufwritepost ~/vimfiles/vimrc source ~/vimfiles/vimrc

" turn off search hilight when redraw screen
nnoremap <C-L> :noh<cr><C-L>

" https://shapeshed.com/vim-netrw/
" vertical split explorer
command! Vex Vexplore
map <leader>vex :Vex<cr>
let g:netrw_browse_split = 4
let g:netrw_winsize = 25
let g:netrw_liststyle = 3
let g:netrw_altv = 1
augroup ProjectDrawer
    autocmd!
    autocmd VimEnter * :Vexplore
augroup END


"""""""""
"" ui

"disable scrollbars
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L

set showcmd

set wildmenu
set wildmode=list:full
set wildignore=*.o,*.~,*.pyc
"if has("win16") || has("win32")
"    set wildignore+=.git\*,.hg\*,.svn\*
"else
set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
"endif

" show current position
set ruler

" show line numbers
set number
"set cpoptions+=n
set norelativenumber

" a bit extra margin to the left
set foldcolumn=1

set modeline
set modelines=1

set cmdheight=1

hi User1 ctermbg=white ctermfg=red guibg=white guifg=red

set laststatus=2
set statusline=\ %L\ ߹\ %c\ ߹\ #%n\ «\ %F\ »\ %m%r%h%w
set statusline+=%=
set statusline+=«\ %{strftime(\"%c\",getftime(expand(\"%:p\")))}\ »
set statusline+=\ %Y\ ߹\ %{&fileformat}\ ߹\ %{&fileencoding?&fileencoding:&encoding}\ \ 

set bs=2
set backspace=eol,start,indent
set whichwrap+=<,>,h,l,[,]

" when searching
set ignorecase
set smartcase
"set hlsearch
"set incsearch

" for regexp
set magic

" show matching brackets
set showmatch
" blink match for x seconds
set mat=2

set noerrorbells
set novisualbell
set t_vb=
set tm=500

set expandtab
set smarttab
set softtabstop=4
set shiftwidth=4
set tabstop=4

set autoindent
set smartindent
set cindent

set wrap

set textwidth=100


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => plugins



"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => helper functions

function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    endif
    return ''
endfunction

function! GitBranch()
    return system("git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -d '\n'")
endfunction

function! StatuslineGit()
    let l:brachname = GitBranch()
    return strlen(l:branchname) > 0?' '.l:branchname.' ':''
endfunction

function! LinterStatus() abort
   let l:counts = ale#statusline#Count(bufnr(''))
   let l:all_errors = l:counts.error + l:counts.style_error
   let l:all_non_errors = l:counts.total - l:all_errors
   return l:counts.total == 0 ? '' : printf(
   \ 'W:%d E:%d',
   \ l:all_non_errors,
   \ l:all_errors
   \)
endfunction

" Restore screen position
" https://vim.fandom.com/wiki/Restore_screen_size_and_position 
let g:screen_size_restore_pos = 1

if has("gui_running")
  function! ScreenFilename()
    if has('amiga')
      return "s:.vimsize"
    elseif has('win32')
      return $HOME.'\_vimsize'
    else
      return $HOME.'/.vimsize'
    endif
  endfunction

  function! ScreenRestore()
    " Restore window size (columns and lines) and position
    " from values stored in vimsize file.
    " Must set font first so columns and lines are based on font size.
    let f = ScreenFilename()
    if has("gui_running") && g:screen_size_restore_pos && filereadable(f)
      let vim_instance = (g:screen_size_by_vim_instance==1?(v:servername):'GVIM')
      for line in readfile(f)
        let sizepos = split(line)
        if len(sizepos) == 5 && sizepos[0] == vim_instance
          silent! execute "set columns=".sizepos[1]." lines=".sizepos[2]
          silent! execute "winpos ".sizepos[3]." ".sizepos[4]
          return
        endif
      endfor
    endif
  endfunction

  function! ScreenSave()
    " Save window size and position.
    if has("gui_running") && g:screen_size_restore_pos
      let vim_instance = (g:screen_size_by_vim_instance==1?(v:servername):'GVIM')
      let data = vim_instance . ' ' . &columns . ' ' . &lines . ' ' .
            \ (getwinposx()<0?0:getwinposx()) . ' ' .
            \ (getwinposy()<0?0:getwinposy())
      let f = ScreenFilename()
      if filereadable(f)
        let lines = readfile(f)
        call filter(lines, "v:val !~ '^" . vim_instance . "\\>'")
        call add(lines, data)
      else
        let lines = [data]
      endif
      call writefile(lines, f)
    endif
  endfunction

  if !exists('g:screen_size_restore_pos')
    let g:screen_size_restore_pos = 1
  endif
  if !exists('g:screen_size_by_vim_instance')
    let g:screen_size_by_vim_instance = 1
  endif
  autocmd VimEnter * if g:screen_size_restore_pos == 1 | call ScreenRestore() | endif
  autocmd VimLeavePre * if g:screen_size_restore_pos == 1 | call ScreenSave() | endif
endif

