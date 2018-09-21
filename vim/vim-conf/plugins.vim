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

" 2. TeX plugin
if !exists('g:disable_vimtex')
    Bundle 'lervag/vimtex'
endif

" 3. Tagbar
if !exists('g:disable_tagbar')
    Bundle 'majutsushi/tagbar'
endif

" 4. Markdown syntax
if !exists('g:disable_vim_markdown')
    Bundle 'plasticboy/vim-markdown'
endif

" 5. Tabular
if !exists('g:disable_tabular')
    Bundle 'godlygeek/tabular'
endif

" NERDTree File Manager
if !exists('g:disable_nerdtree')
    Bundle 'scrooloose/nerdtree'
endif

" HTML,CSS Completation
if !exists('g:disable_emmet_vim')
    Bundle 'mattn/emmet-vim'
endif

" 7. YCM
" Only load YCM when python is supported, and YCM DON'T support Android Termux
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

" 16. webapi
if !exists('g:disable_webapi_vim')
    Bundle 'mattn/webapi-vim'
endif

" 17. vim-shell
if !exists('g:disable_vim_shell')
    Bundle 'xolox/vim-shell'
endif

" 18. vim-misc
if !exists('g:disable_vim_misc')
    Bundle 'xolox/vim-misc'
endif

" 19. vim-session
if !exists('g:disable_vim_session')
    Bundle 'xolox/vim-session'
endif

" 20. vim-notes
if !exists('g:disable_vim_notes')
    Bundle 'xolox/vim-notes'
endif
" }}}

" {{{ Plugin End

" End Plugins
call vundle#end()

" }}}

" {{{ My Plugins Begin

" plugins list
let plugins_list_raw = [
            \"$LOADPATH/self-plugins/markdown-tools",
            \"$LOADPATH/self-plugins/various-exec",
            \"$LOADPATH/self-plugins/asy-compile"
            \]
let plugins_list_abs = map(plugins_list_raw, "expand(v:val)")

for plugpath in plugins_list_abs
    exec "set rtp+=".plugpath
endfor

" autocmd FileType markdown if filereadable(expand('~/.vim/self-plugins/markdown-contents.vim')) | source ~/.vim/self-plugins/markdown-contents.vim | endif

" }}} My Plugins End
