" Description: Markdown tools

if has('markdown_tools_loaded')
    finish
endif

let g:markdown_tools_loaded = 1

command! -nargs=0 MakeTOC
\ call markdown_toc#gen_toc_to_reg(v:register)

" shortcut
autocmd Filetype markdown nnoremap <silent><buffer><leader>toc :MakeTOC<cr>
