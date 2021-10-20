" Plugins configure

" airline "[[
" let g:airline_theme='random'
"]]

function! s:nerdtreeConfig() "[[
    if filereadable(expand("$HOME/.vim/bundle/nerdtree/plugin/NERD_tree.vim"))
        " 没有指定文件打开vim默认界面为NERDTee
        autocmd StdinReadPre * let s:std_in=1
        if g:islinux
            autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree
                        \ | endif
        elseif g:iswindows
            autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree ~/
                        \ | endif
        endif

        " NERDTree Toggle
        nnoremap <leader>n :NERDTreeToggle<cr>
        nnoremap <c-n> :NERDTreeToggle<cr>
        nnoremap <F7> :NERDTreeToggle<cr>
    endif
endfunction "]]
autocmd User nerdtree-loaded call s:nerdtreeConfig()

" UltiSnips [[
let g:ycm_use_ultisnips_comleter = 1        " YCM使用ultisnips补全,默认值
let g:UltiSnipsExpandTrigger="<c-u>"
" let g:UltiSnipsListSnippets="<c-l>"
let g:UltiSnipsJumpForwardTrigger="<c-o>"
let g:UltiSnipsJumpBackwardTrigger="<c-i>"
set rtp+=~/.vim/UltiSnips
autocmd BufRead *.snippets set filetype=snippets
" ]]

" YCM [[
" default ycm_extra_conf
let g:ycm_global_ycm_extra_conf = expand('~/.ycm_extra_conf.py')
function! Strip(input_string)
    return substitute(a:input_string, '^[\s\n\r]*\(.\{-}\)[\r\n\s]*$', '\1', '')
endfunction
" turn off the tip in load .ycm_extra_conf.py
let g:ycm_confirm_extra_conf = 0
let g:ycm_server_python_interpreter = Strip(system('which python3'))

" update location-list every time when YCMDiags run
let g:ycm_always_populate_location_list=1
let g:ycm_key_invoke_completion = '<C-Space>'

function EnableYCMShortcuts() 
    nnoremap <buffer><F5>        :YcmForceCompileAndDiagnostics<cr>
    nnoremap <buffer><leader>yd  :YcmShowDetailedDiagnostics<cr>
    nnoremap <buffer><leader>gd  :YcmCompleter GoToDeclaration<cr>
    nnoremap <buffer><leader>gD  :YcmCompleter GoToDefinition<cr>
    nnoremap <buffer><leader>gi  :YcmCompleter GoToImplementation<cr>
    nnoremap <buffer><leader>gc  :YcmCompleter GoToInclude<cr>
    nnoremap <buffer><leader>go  :YcmCompleter GoTo<cr>
    nnoremap <buffer><leader>gt  :YcmCompleter GetType<cr>
    nnoremap <buffer><leader>gr  :YcmCompleter GoToReferences<cr>
    nnoremap <buffer><leader>y=  :YcmCompleter Format<cr>
    nnoremap <buffer><leader>yf  :YcmCompleter FixIt<cr>
    nnoremap <buffer><leader>ry  :YcmCompleter RestartServer<cr>
endfunction

autocmd! FileType c,cpp,typescript,javascript,rust,java call EnableYCMShortcuts()

" disable ycm in following filetype
let g:ycm_filetype_blacklist = {
      \ 'lua' : 1,
      \ 'vim' : 1,
      \ 'bash' : 1,
      \ 'python' : 1,
      \
      \ 'tagbar' : 1,
      \ 'qf' : 1,
      \ 'notes' : 1,
      \ 'unite' : 1,
      \ 'text' : 1,
      \ 'vimwiki' : 1,
      \ 'pandoc' : 1,
      \ 'infolog' : 1,
      \ 'mail' : 1
      \ }
let g:ycm_semantic_triggers = {
            \ 'css': [ 're!^', 're!^\s+', ': ' ],
            \ 'scss': [ 're!^', 're!^\s+', ': ' ],
            \ }
" ]]

" PowerShell filetype [[
augroup filetypedetect
    au BufNewFile,BufRead *.ps1 set filetype=ps1
augroup END

let g:ps1_nofold_sig = 1
let g:ps1_nofold_blocks = 1
" ]]

"[[ minibufexpl
nnoremap <leader>mbe :MBEOpen<cr>
nnoremap <leader>mbc :MBEClose<cr>
nnoremap <leader>mbt :MBEToggle<cr>
"]]

" Easy-Align
vmap <Enter> <Plug>(EasyAlign)

" Ack
nnoremap <leader>t :Ack \(FIXME\)\\|\(TODO\)<cr>

function! s:telescopeConfig() "[[
    nnoremap <leader>ff <cmd>Telescope find_files<cr>
    nnoremap <leader>fg <cmd>Telescope live_grep<cr>
    nnoremap <leader>fb <cmd>Telescope buffers<cr>
    nnoremap <leader>fh <cmd>Telescope help_tags<cr>
endfunction "]]
autocmd! User telescope.nvim-loaded call s:telescopeConfig()

function! s:whichKeyConfig() "[[
    nnoremap <silent>, :WhichKey '*'<CR>
    set timeoutlen=400
    call which_key#register('*', "g:which_key_map")
endfunction "]]
autocmd! User vim-which-key-loaded call s:whichKeyConfig()

function! s:completionnvimConfig() "[[
    set shortmess+=c
    set completeopt=menuone,noinsert,noselect
    let g:completion_enable_auto_popup = 1
    let g:completion_trigger_on_delete = 1
    let g:completion_trigger_character = ['.', '::', '->']
    let g:completion_enable_snippet='UltiSnips'
    let g:completion_trigger_keyword_length=2
    let g:completion_confirm_key = ""
    let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy']
    imap <silent> <c-space> <Plug>(completion_trigger)
endfunction "]]
autocmd! User completion-nvim-loaded call s:completionnvimConfig()

function! s:vimColorsXcode() "[[
    colorscheme xcodewwdc
endfunction "]]
autocmd! User vim-colors-xcode-loaded call s:vimColorsXcode()

function! s:luadevConfig() "[[
    augroup LuadevMapping
        autocmd! FileType lua nmap <buffer><leader>lx <Plug>(Luadev-RunLine) 
                    \ |       vmap <buffer>lx <Plug>(Luadev-Run)
    augroup end
endfunction "]]
autocmd! User nvim-luadev-loaded call s:luadevConfig()

function!s:nvimReplConfig() "[[
    nmap <leader>ax <Plug>(nvim-repl-current-line)
    nmap <leader>af <Plug>(nvim-repl-current-file)
    vmap <silent>aa <Plug>(nvim-repl-selection)

    nmap <leader>ar <Plug>(nvim-repl-reset-interpreter)

    nmap <leader>ac <Plug>(nvim-repl-win-close)
    nmap <leader>ao <Plug>(nvim-repl-win-open)
    nmap <leader>at <Plug>(nvim-repl-win-toggle)
    nmap <leader>al <Plug>(nvim-repl-buffer-clear)
    nmap <leader>as <Plug>(nvim-repl-buffer-close)

    nmap <leader>am <Plug>(nvim-repl-toggle-internal-external-mode)
    nmap <leader>ap <Plug>(nvim-repl-show-prompt)
endfunction "]]
autocmd! User nvim-repl-loaded call s:nvimReplConfig()

function!s:emmetVim() "[[
    imap <C-y>; <plug>(emmet-expand-abbr)
endfunction "]]
autocmd! User emmet-vim-loaded call s:emmetVim()

" vim-clang-format [[
let g:clang_format#style_options = {
            \ "AccessModifierOffset" : -4,
            \ "AllowShortIfStatementsOnASingleLine" : "true",
            \ "AlwaysBreakTemplateDeclarations" : "true",
            \ "Standard" : "C++11"}
let g:clang_format#detect_style_file = 1

" map to <Leader>cf in C++ code
autocmd FileType c,cpp,objc nnoremap <buffer><Leader>cf :<C-u>ClangFormat<CR>
autocmd FileType c,cpp,objc vnoremap <buffer><Leader>cf :ClangFormat<CR>
" ]]

