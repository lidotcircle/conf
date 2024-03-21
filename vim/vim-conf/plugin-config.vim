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

if !has("nvim")
autocmd! FileType c,cpp,typescript,javascript,rust,java call EnableYCMShortcuts()
endif

" disable ycm in following filetype
let g:ycm_filetype_blacklist = {
      \ 'lua' : 1,
      \ 'vim' : 1,
      \ 'bash' : 1,
      \ 'python' : 1,
      \
      \ 'c' : 1,
      \ 'cpp' : 1,
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
nnoremap <leader>tt :Ack \(FIXME\)\\|\(TODO\)<cr>

function! s:telescopeConfig() "[[
    nnoremap <leader>fs <cmd>Telescope<cr>
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

function!s:gitgutter() "[[
    nnoremap <leader>hp :GitGutterPrevHunk<cr>
    nnoremap <leader>hn :GitGutterNextHunk<cr>
    nnoremap <leader>hq :GitGutterQuickFixCurrentFile<cr>
    nnoremap <leader>hQ :GitGutterQuickFix<cr>
    nnoremap <leader>hg :GitGutterToggle<cr>
    nnoremap <leader>hs :GitGutterStageHunk<cr>
    nnoremap <leader>hu :GitGutterUndoHunk<cr>
    nnoremap <leader>hv :GitGutterPreviewHunk<cr>
    nnoremap <leader>hf :GitGutterFold<cr>
endfunction "]]
autocmd! User vim-gitgutter-loaded call s:gitgutter()

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

function!s:vimTranslator() "[[
    nnoremap <leader>th :TranslateW --target_lang=zh<cr>
    nnoremap <leader>te :TranslateW --target_lang=en<cr>
    vnoremap th         :TranslateW --target_lang=zh<cr>
    vnoremap te         :TranslateW --target_lang=en<cr>
    vnoremap trh        :TranslateR --target_lang=zh<cr>
    vnoremap tre        :TranslateR --target_lang=en<cr>
endfunction "]]
autocmd! User vim-translator-loaded call s:vimTranslator()

function!s:troubleNvim() "[[
    nnoremap <leader>xx <cmd>TroubleToggle<cr>
    nnoremap <leader>xw <cmd>TroubleToggle workspace_diagnostics<cr>
    nnoremap <leader>xd <cmd>TroubleToggle document_diagnostics<cr>
    nnoremap <leader>xq <cmd>TroubleToggle quickfix<cr>
    nnoremap <leader>xl <cmd>TroubleToggle loclist<cr>
    nnoremap <leader>gR <cmd>TroubleToggle lsp_references<cr>
endfunction "]]
autocmd! User trouble.nvim-loaded call s:troubleNvim()

function!s:nvimdap() "[[
    nnoremap <silent> <leader>lc <Cmd>lua require'dap'.continue()<CR>
    nnoremap <silent> <leader>ln <Cmd>lua require'dap'.step_over()<CR>
    nnoremap <silent> <leader>ls <Cmd>lua require'dap'.step_into()<CR>
    nnoremap <silent> <leader>lo <Cmd>lua require'dap'.step_out()<CR>
    nnoremap <silent> <Leader>lb <Cmd>lua require'dap'.toggle_breakpoint()<CR>
    nnoremap <silent> <Leader>lB <Cmd>lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>
    nnoremap <silent> <Leader>lp <Cmd>lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>
    nnoremap <silent> <Leader>dr <Cmd>lua require'dap'.repl.open()<CR>
    nnoremap <silent> <Leader>dl <Cmd>lua require'dap'.run_last()<CR>
endfunction "]]
autocmd! User nvim-dap-loaded call s:nvimdap()

function!s:nvimdappython() "[[
    lua require('dap-python').setup('/usr/bin/python3')
    nnoremap <silent> <leader>dn :lua require('dap-python').test_method()<CR>
    nnoremap <silent> <leader>df :lua require('dap-python').test_class()<CR>
    vnoremap <silent> <leader>ds <ESC>:lua require('dap-python').debug_selection()<CR>
endfunction "]]
autocmd! User nvim-dap-python-loaded call s:nvimdappython()

function!s:neogit() "[[
    nnoremap <silent> <leader>gg :Neogit<CR>
endfunction "]]
autocmd! User neogit-loaded call s:neogit()

function!s:monokai() "[[
    lua require('monokai').setup { palette = require('monokai').pro }
endfunction "]]
autocmd! User monokai.nvim-loaded call s:monokai()

function!s:nvimtree() "[[
    lua require('nvim-tree').setup{ }
    nnoremap <silent> <leader>n :NvimTreeToggle<CR>
endfunction "]]
autocmd! User nvim-tree.lua-loaded call s:nvimtree()

function!s:symbolsOutline() "[[
    lua require('symbols-outline').setup { }
    nnoremap <silent> <leader>so :SymbolsOutline<CR>
endfunction "]]
autocmd! User symbols-outline.nvim-loaded call s:symbolsOutline()

function!s:webDevicons() "[[
    lua require('nvim-web-devicons').setup { }
endfunction "]]
autocmd! User nvim-web-devicons-loaded call s:webDevicons()

function!s:barbar() "[[
    lua require('barbar').setup { }
    " Move to previous/next
    nnoremap <silent>    <A-,> <Cmd>BufferPrevious<CR>
    nnoremap <silent>    <A-.> <Cmd>BufferNext<CR>

    " Re-order to previous/next
    nnoremap <silent>    <A-<> <Cmd>BufferMovePrevious<CR>
    nnoremap <silent>    <A->> <Cmd>BufferMoveNext<CR>

    " Goto buffer in position...
    nnoremap <silent>    <A-1> <Cmd>BufferGoto 1<CR>
    nnoremap <silent>    <A-2> <Cmd>BufferGoto 2<CR>
    nnoremap <silent>    <A-3> <Cmd>BufferGoto 3<CR>
    nnoremap <silent>    <A-4> <Cmd>BufferGoto 4<CR>
    nnoremap <silent>    <A-5> <Cmd>BufferGoto 5<CR>
    nnoremap <silent>    <A-6> <Cmd>BufferGoto 6<CR>
    nnoremap <silent>    <A-7> <Cmd>BufferGoto 7<CR>
    nnoremap <silent>    <A-8> <Cmd>BufferGoto 8<CR>
    nnoremap <silent>    <A-9> <Cmd>BufferGoto 9<CR>
    nnoremap <silent>    <A-0> <Cmd>BufferLast<CR>

    " Pin/unpin buffer
    nnoremap <silent>    <A-p> <Cmd>BufferPin<CR>

    " Close buffer
    nnoremap <silent>    <A-c> <Cmd>BufferClose<CR>
    " Restore buffer
    nnoremap <silent>    <A-s-c> <Cmd>BufferRestore<CR>

    " Wipeout buffer
    "                          :BufferWipeout
    " Close commands
    "                          :BufferCloseAllButCurrent
    "                          :BufferCloseAllButVisible
    "                          :BufferCloseAllButPinned
    "                          :BufferCloseAllButCurrentOrPinned
    "                          :BufferCloseBuffersLeft
    "                          :BufferCloseBuffersRight

    " Magic buffer-picking mode
    nnoremap <silent> <C-p>    <Cmd>BufferPick<CR>
    nnoremap <silent> <C-p>    <Cmd>BufferPickDelete<CR>

    " Sort automatically by...
    nnoremap <silent> <Space>bb <Cmd>BufferOrderByBufferNumber<CR>
    nnoremap <silent> <Space>bd <Cmd>BufferOrderByDirectory<CR>
    nnoremap <silent> <Space>bl <Cmd>BufferOrderByLanguage<CR>
    nnoremap <silent> <Space>bw <Cmd>BufferOrderByWindowNumber<CR>
endfunction "]]
autocmd! User barbar.nvim-loaded call s:barbar()

function!s:telescopeRecentFiles() "[[
    lua require('telescope').load_extension("recent_files")
    lua vim.api.nvim_set_keymap("n", "<Leader><Leader>", [[<cmd>lua require('telescope').extensions.recent_files.pick()<CR>]], {noremap = true, silent = true})
endfunction "]]
autocmd! User telescope-recent-files-loaded call s:telescopeRecentFiles()

function!s:tabby() "[[
    lua require('tabby').setup()
endfunction "]]
autocmd! User tabby.nvim-loaded call s:tabby()

let g:session_autosave = 'no'
