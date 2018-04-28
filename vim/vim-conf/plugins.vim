" {{{ Plugin Begin

" runtimepath
set rtp+=$LOADPATH/bundle/Vundle.vim

" Begin Plugins
call vundle#begin()
call vundle#rc()

"let vundle as plugin manager
Bundle 'VundleVim/Vundle.vim'

" }}}

" Plugins list {{{

" 1. Statusline
if !exists('g:disable_vim_powerline')
    Bundle 'Lokaltog/vim-powerline'
endif

" 2. TeX插件
if !exists('g:disable_vimtex')
    Bundle 'lervag/vimtex'
endif

" 3. Tagbar
if !exists('g:disable_tagbar')
    Bundle 'majutsushi/tagbar'
endif

" 4. Vim-markdown Markdown语法
if !exists('g:disable_vim_markdown')
    Bundle 'plasticboy/vim-markdown'
endif

" 5. Tabular
if !exists('g:disable_tabular')
    Bundle 'godlygeek/tabular'
endif

" 6. NERDTree文件浏览
if !exists('g:disable_nerdtree')
    Bundle 'scrooloose/nerdtree'
endif

" HTML Complete, Emmet
if !exists('g:disable_emmet_vim')
    Bundle 'mattn/emmet-vim'
endif

" 7. YCM
" 只在python支持加载YCM, and YCM DON'T support Android Termux
if (has('python') || has('python3')) && !(g:isandroid) && !exists('g:disable_YouCompleteMe')
    Bundle 'Valloric/YouCompleteMe'
endif

" 8. Python mode
" Unkown reason for pymode work always error in windows ...
if g:islinux && !exists('g:disable_python_mode')
    Bundle 'klen/python-mode'
endif

" 9. ctrlp
if !exists('g:disable_ctrlp.vim')
    Bundle 'kien/ctrlp.vim'
endif

" 10. Conque-GDB
"
let g:load_conque_gdb = 1
function! s:conque_gdb() " {{{
    if g:islinux && (g:load_conque_gdb == 1) && !exists('g:disable_Conque_GDB')
        Bundle 'vim-scripts/Conque-GDB'
        let g:load_conque_gdb = 0
    endif
endfunction " }}}
autocmd FileType c,cpp call s:conque_gdb()

" 11. ultisnips
if has('python') || has('python3') && !exists('g:disable_ultsnips')
    Bundle 'SirVer/ultisnips'
endif

" 12. powershell syntax and indent
if !exists('g:disable_vim_ps1')
    Bundle 'PProvost/vim-ps1'
endif

" 13. minibufexpl instead of bufexplore
" Bundle 'jlanzarotta/bufexplorer'
if !exists('g:disable_minibufexpl_vim')
    Bundle 'fholgado/minibufexpl.vim'
endif

" 14. haskell-vim
if !exists('g:disable_haskell_vim')
    Bundle 'neovimhaskell/haskell-vim'
endif

" 15. jedi-vim
if !exists('g:disable_jedi_vim')
    Bundle 'davidhalter/jedi-vim'
endif

" }}}

" {{{ Plugin End

" End Plugins
call vundle#end()

" }}}

" {{{ My Plugins Begin

" Markdown Generate Contents
autocmd FileType markdown if filereadable(expand('~/.vim/self-plugins/markdown-contents.vim')) | source ~/.vim/self-plugins/markdown-contents.vim | endif

" }}} My Plugins End
