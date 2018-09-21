" Description: auto compile when file write

if has('g:asy_cc_loaded')
    finish
endif
let g:asy_cc_loaded = 1

autocmd! BufWritePost *.asy call asyMain#Compile(expand("%:t")) 

" clean b:already_start_viewer
function! CleanViewerVar() "{{{
    if exists('b:already_start_viewer')
        unlet b:already_start_viewer
    endif
endfunction "}}}
nnoremap <silent><leader>av :call CleanViewerVar()<cr>
