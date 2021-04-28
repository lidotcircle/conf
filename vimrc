" FileName: .vimrc
" FileType: vim
" Description: First loaded file to config vim

" Avoid to load this file cyclicly.
if exists('g:vimrc_loaded')
    finish
endif
let g:vimrc_loaded = 1

" OS Type[[

"" Set some environment variables, for detemining current
"" running environment
let g:islinux = v:false
let g:iswindows = v:false

if (has("win32")||has("win64")||has("win95")||has("win16"))
    let g:iswindows = v:true
elseif (has('unix'))
    let g:islinux = v:true
endif

" ]]


let loadpath = expand('~/.vim')

"" Add '~/.vim/' to runtime path
if (iswindows)
    execute 'set runtimepath+='.loadpath
endif


let vimfiles = [
            \ "/extra_pre.vim",
            \ "/vim-conf/set.vim",
            \ "/vim-conf/map.vim",
            \ "/vim-conf/colors.vim",
            \ "/vim-conf/commands.vim",
            \ "/vim-conf/plugin-config.vim",
            \ "/vim-conf/plugins.vim",
            \ "/extra_post.vim",
            \ ]

for vfile in vimfiles
    let vff = loadpath.vfile
    if filereadable(vff)
        execute 'source '.vff
    endif
endfor

