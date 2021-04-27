" FileName: .vimrc
" FileType: vim
" Description: First loaded file to config vim

" Avoid to load this file cyclicly.
if exists('g:vimrc_loaded')
    finish
endif
let g:vimrc_loaded = 1

" OS Type{{{

"" Set some environment variables, for detemining current
"" running environment
let g:islinux = 0
let g:iswindows = 0
let g:isandroid = 0

if (has("win32")||has("win64")||has("win95")||has("win16"))
    let g:iswindows = 1
elseif (has('unix'))
    let g:islinux = 1
    if !empty(system("uname -a | grep 'Android'"))
        let g:isandroid = 1
    endif
endif

" }}}

" Functions for loading files {{{

" FuncName: LoadDir
function! s:LoadDir(_dir) "{{{
    echo "Hello"
endfunc "}}}

" End loading functions }}}

" Load config files {{{

"" Add '~/.vim/' to runtime path
if (iswindows)
    set rtp+=~/.vim
endif

"" Set loading path
let $LOADPATH='~/.vim'

""" before source main configure
if filereadable(expand($LOADPATH."/extra_pre.vim"))
    source $LOADPATH/extra_pre.vim
endif

" Main configure files {{{
""" set.vim
if filereadable(expand($LOADPATH."/vim-conf/set.vim"))
    source $LOADPATH/vim-conf/set.vim
endif
""" map.vim
if filereadable(expand($LOADPATH."/vim-conf/map.vim"))
    source $LOADPATH/vim-conf/map.vim
endif
""" abbreviate.vim
if filereadable(expand($LOADPATH."/vim-conf/abbreviate.vim"))
    source $LOADPATH/vim-conf/abbreviate.vim
endif
""" colors.vim
if filereadable(expand($LOADPATH."/vim-conf/colors.vim"))
    source $LOADPATH/vim-conf/colors.vim
endif
""" plugins.vim
if filereadable(expand($LOADPATH."/vim-conf/plugins.vim"))
    source $LOADPATH/vim-conf/plugins.vim
endif
""" plugin-config.vim
if filereadable(expand($LOADPATH."/vim-conf/plugin-config.vim"))
    source $LOADPATH/vim-conf/plugin-config.vim
endif

""" commands.vim
if filereadable(expand($LOADPATH."/vim-conf/commands.vim"))
    source $LOADPATH/vim-conf/commands.vim
endif
" End Main configure files}}}

""" after source main configure
if filereadable(expand($LOADPATH."/extra_post.vim"))
    source $LOADPATH/extra_post.vim
endif

" End Configure}}} 
