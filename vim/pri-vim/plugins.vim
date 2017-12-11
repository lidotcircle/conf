" {{{ Plugin Begin

" runtimepath
set rtp+=~/$MYRTP/bundle/Vundle.vim

" Begin Plugins
call vundle#begin()
call vundle#rc()

"let vundle as plugin manager
Bundle 'VundleVim/Vundle.vim'

" }}}

" Plugins list {{{

" 1. Statusline
Bundle 'Lokaltog/vim-powerline'

" 2. TeX插件
Bundle 'lervag/vimtex'

" 3. Tagbar
Bundle 'majutsushi/tagbar'

" 4. Vim-markdown Markdown语法
Bundle 'plasticboy/vim-markdown'

" 5. Tabular
Bundle 'godlygeek/tabular'

" 6. NERDTree文件浏览
Bundle 'scrooloose/nerdtree'

" HTML Complete, Emmet
Bundle 'mattn/emmet-vim'

" 7. YCM
" 只在python支持加载YCM
if has('python') || has('python3')
    Bundle 'Valloric/YouCompleteMe'
endif

" 8. Python mode
" Unkown reason for pymode work always error in windows ...
if g:islinux
    Bundle 'klen/python-mode'
endif

" 9. ctrlp
Bundle 'kien/ctrlp.vim'

" 10. Conque-GDB
"
let g:load_conque_gdb = 1
function! s:conque_gdb() " {{{
if g:islinux && (g:load_conque_gdb == 1)
    Bundle 'vim-scripts/Conque-GDB'
    let g:load_conque_gdb = 0
endif
endfunction " }}}
autocmd FileType c,cpp call s:conque_gdb()

" 11. ultisnips
if has('python') || has('python3')
    Bundle 'SirVer/ultisnips'
endif

" 12. powershell syntax and indent
Bundle 'PProvost/vim-ps1'

" 13. bufexplore
Bundle 'jlanzarotta/bufexplorer'

" 14. haskell-vim
Bundle 'neovimhaskell/haskell-vim'

" }}}

" {{{ Plugin End

" End Plugins
call vundle#end()

" }}}

" {{{ My Plugins Begin

" Markdown Generate Contents
autocmd FileType markdown if filereadable(expand('~/.vim/pri-plugins/markdown-contents.vim')) | source ~/.vim/pri-plugins/markdown-contents.vim | endif

" }}} My Plugins End
