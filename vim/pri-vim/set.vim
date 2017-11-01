" 重要 {{{

"不和vi兼容
set nocompatible
set backspace=indent,eol,start

"开启文件类型检测
filetype on
"根据检测到文件加载对应的插件
filetype plugin on

"开启语法高亮
syntax enable
"允许指定语法高亮替换
syntax on

" 代码智能缩进
filetype indent on

"map leader设置为,
let mapleader = ","

" 编码设置为utf-8
set encoding=utf-8

" 文件格式设置为 unix
autocmd BufReadPost * if &modifiable | set fileformat=unix | endif

" }}}

" 显示界面 {{{

" 左侧行号显示
set number
set numberwidth=4

" 折行
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

" 字体
if has('gui_running')
    set columns=110 lines=60
    if g:islinux
        set guifont=Bitstream\ Vera\ Sans\ Mono\ 14
    elseif g:iswindows
        set guifont=Bitstream_Vera_Sans_Mono:h11
    endif
else
    set columns=120 lines=60
endif

" }}}

" 输入,语法,缩进 {{{

" <Tab>的有关设置
set tabstop=4
set shiftwidth=4
set expandtab
set softtabstop=4

" 实时搜索
set incsearch
" 大小写不敏感
set ignorecase
" vim 自身命令行智能补全
set wildmenu

" }}}

"设置自动写在buffer跳转的操作下
set autowrite

" 无swap文件
set noswapfile
