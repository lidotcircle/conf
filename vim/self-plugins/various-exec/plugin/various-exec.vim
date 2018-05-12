" Description: execute the expression in v:register

if has('g:various_exec_loaded')
    finish
endif
let g:various_exec_loaded = 1

" Execute v:register
autocmd FileType vim nnoremap <buffer><silent><leader>ev 
            \:call exec_register#execute_v_register()<cr>
