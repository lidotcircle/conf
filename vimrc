" 判别系统 {{{

let g:islinux = 0
let g:iswindows = 0

if (has("win32")||has("win64")||has("win95")||has("win16"))
    let g:iswindows = 1
elseif (has('unix'))
    let g:islinux = 1
endif

" }}}

"加载其他的个人配置 {{{

""设置个环境变量
if(islinux)
    let $MYRTP='.vim'
else
    if (filereadable(expand("~/.vim/bundle/Vundle.vim/README.md")))
        set rtp+=~/.vim         "color需要
        let $MYRTP = '.vim'
    else
        let $MYRTP = 'vimfiles'
    endif
endif

""" set.vim
if filereadable(expand("~/$MYRTP/pri-vim/set.vim"))
    source ~/$MYRTP/pri-vim/set.vim
endif
""" map.vim
if filereadable(expand("~/$MYRTP/pri-vim/map.vim"))
    source ~/$MYRTP/pri-vim/map.vim
endif
""" abbreviate.vim
if filereadable(expand("~/s:MYRTP/pri-vim/abbreviate.vim"))
    source ~/$MYRTP/pri-vim/abbreviate.vim
endif
""" colors.vim
if filereadable(expand("~/$MYRTP/pri-vim/colors.vim"))
    source ~/$MYRTP/pri-vim/colors.vim
endif
""" plugins.vim
if filereadable(expand("~/$MYRTP/pri-vim/plugins.vim"))
    source ~/$MYRTP/pri-vim/plugins.vim
endif
""" plugin-config.vim
if filereadable(expand("~/$MYRTP/pri-vim/plugin-config.vim"))
    source ~/$MYRTP/pri-vim/plugin-config.vim
endif

""" commands.vim
if filereadable(expand("~/$MYRTP/pri-vim/commands.vim"))
    source ~/$MYRTP/pri-vim/commands.vim
endif
" }}} 
