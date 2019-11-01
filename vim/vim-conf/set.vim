" Setting

" Important {{{

" nocompatible with vi
set nocompatible
set backspace=indent,eol,start

" turn on file type check
filetype on
" according to file type to load plugin
filetype plugin on
" indent, accroding to filetype
filetype indent on

" turn on syntax
syntax enable
syntax on

" the <mapleader>
let mapleader = ","

" file encoding
set encoding=utf-8

" set fileformat to unix
autocmd BufReadPost * if &modifiable | set fileformat=unix | endif

" linespace
set linespace=5

" indnet
set cindent

" message
set cmdheight=2

" }}}

" Display {{{

" Line number in left side
set number
set numberwidth=4

" wrap the line
set wrap

" min column
set winwidth=20

" 括号匹配,闪烁频率
set showmatch
set matchtime=3

"statusline的相关的设置
set laststatus=2
"set statusline=
" 显示当前光标位置
set ruler
" 高亮显示当前行/列
set cursorline
set cursorcolumn
" 高亮显示搜索结果
set hlsearch

" 禁止光标闪烁
set gcr=a:block-blinkon0
" 禁止显示滚动条
set guioptions-=l
set guioptions-=L
set guioptions-=r
set guioptions-=R
" 禁止显示菜单和工具条
set guioptions-=m
set guioptions-=T

" Font
if has('gui_running')
    set columns=110 lines=60
    if g:islinux
        set guifont=Bitstream\ Vera\ Sans\ Mono\ 14
    elseif g:iswindows
        set guifont=Bitstream_Vera_Sans_Mono:h11
    endif
endif

" }}}

" Miscellaneous {{{

" <TAB>与缩进的有关设置
"" <TAB>的显示空格数
set tabstop=4
"" 语法缩进的单位
set shiftwidth=4
"" 是否以空格代替<TAB>
set expandtab
"" 用于调整插入<TAB>时插入的缩进量 = (softtabstop/tabstop)*<TAB> + (softtabstop%tabstop)*<SPACE>
set softtabstop=4

" 实时搜索
set incsearch
" 大小写不敏感
set ignorecase
" vim 自身命令行智能补全
set wildmenu

set autowrite
set noswapfile

" }}}

"{{{ TAGS
let s:extraTags = [
            \      $HOME . '/cpython.tag',
            \      $HOME . '/llvmlibcxx.tag']
"            \      $HOME . '/c++stl.tag',
let s:currentTags = &tags
let s:extraTagsString = ""
for s:exT in s:extraTags
    let s:extraTagsString .= "," . s:exT
endfor
exec "set tags=" . s:currentTags . s:extraTagsString
"}}}
