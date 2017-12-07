"   插件配置 {{{

" Statusline {{{
" 设置statusline样式
let g:Powerline_colorscheme='solarized256'
" }}}

" NERDTree {{{
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
    nnoremap <leader>d :NERDTreeToggle<cr>
endif
" gt --- 下一个tab windows(normal mode)
" gT --- 上一个tab windows
" }}}

" Tagbar {{{
nnoremap <silent><F8> :TagbarToggle<cr>
" }}}

" Ctrlp {{{
let g:ctrlp_map = '<c-p>'

" 忽略版本控制的文件
if g:islinux
    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*        " Linux/MacOSX
else
    set wildignore+=*\\.git\\*,*\\.hg\\*,*\\.svn\\*  " Windows ('noshellslash')
endif
" }}}

" Ultisnips {{{
let g:ycm_use_ultisnips_comleter = 1        " YCM使用ultisnips补全,默认值
" }}}

" YCM {{{
" default ycm_extra_conf
let g:ycm_global_ycm_extra_conf =
\'~/.ycm_extra_conf.py'

" turn off the tip in load .ycm_extra_conf.py
let g:ycm_confirm_extra_conf = 0

function! s:ycm_shortcut_c_sopport() " {{{
    " 刷新编译文件
    nnoremap <F5> :YcmForceCompileAndDiagnostics<cr>
    " 查看当前光标下的错误信息
    nnoremap <leader>yd :YcmShowDetailedDiagnostics<cr>
    " 跳转到声明处 subcommand -- GoToDeclaration
    nnoremap <leader>yr :YcmCompleter GoToDeclaration<cr>
    " 跳转到定义出 subcommand -- GoToDefinition
    nnoremap <leader>yf :YcmCompleter GoToDefinition<cr>
endfunction " }}}
" 只在 c,cpp时开启
autocmd Filetype c,cpp call s:ycm_shortcut_c_sopport()

" 关闭YCM在以下类型的开启
let g:ycm_filetype_blacklist = {
      \ 'tagbar' : 1,
      \ 'qf' : 1,
      \ 'notes' : 1,
      \ 'unite' : 1,
      \ 'text' : 1,
      \ 'vimwiki' : 1,
      \ 'pandoc' : 1,
      \ 'infolog' : 1,
      \ 'mail' : 1
      \}
" }}}

" Vimtex {{{
if g:iswindows
    if executable('acrobat')
        let vimtex_view_general_viewer = 'acrobat'
    elseif executable('chrome')
        let vimtex_view_general_viewer = 'chrome'
    endif
elseif g:islinux
    if executable('evince')
        let vim_tex_view_general = 'evince'
    elseif executable('okular')
        let vim_tex_view_general = 'okular'
    elseif executable('google-chrome')
        let vim_tex_view_general = 'google-chrome'
    endif
endif
" }}}

" Vim-ps1 {{{
" PowerShell的文件类型
augroup filetypedetect
    au BufNewFile,BufRead *.ps1 set filetype=ps1
augroup END

let g:ps1_nofold_sig = 1
let g:ps1_nofold_blocks = 1
" }}}

" }}}
