" vimscript的缩写 {{{

function vimscript_abbreviate()
    abbreviate hl highlight
    abbreviate au autocmd
    abbreviate func function
    abbreviate syn syntax
    abbreviate nnm nnoremap
    abbreviate inm inoremap
    abbreviate rnm rnoremap
endfunction

autocmd filetype vim call vimscript_abbreviate()

" }}}

" C, C++的缩写 {{{
"
" }}}
