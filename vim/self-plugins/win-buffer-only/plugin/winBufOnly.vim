
command! -nargs=0 -complete=buffer WinBufOnly
    \ :call winBufferOnly#winBufOnly()

nnoremap <silent><leader>wbo :WinBufOnly<cr>

