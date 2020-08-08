" autocmd

" resource $MYVIMRC, when change some vim sourced files {{{
autocmd BufWritePost $MYVIMRC,~/$MYRTP/pri-vim/* source $MYVIMRC
" }}}

" g:html_view, g:rm_tool, g:eps_viewer, g:svg_viewer{{{
" g:rm_tool
if g:islinux
    let g:rm_tool = 'rm -f'
elseif g:iswindows
    let g:rm_tool = 'del /F'
endif

" g:html_view
if g:islinux
    if executable('google-chrome')
        let g:html_view = 'google-chrome'
    elseif executable('firefox')
        let g:html_view = 'firefox'
    elseif executable('webbrowser-app')
        let g:html_view = 'webbrowser-app'
    endif
elseif g:iswindows
    if executable('chrome')
        let g:html_view = 'chrome'
    elseif executable('firefox')
        let g:html_view = 'firefox'
    endif
endif

" g:eps_viewer
if g:islinux
    if executable('evince')
        let g:eps_viewer = 'evince'
    endif
elseif g:iswindows
    if executable('psv')
        let g:eps_viewer = 'psv'
    endif
endif

" g:svg_viewer
if g:islinux
    if executable('google-chrome')
        let g:svg_viewer = 'google-chrome'
    elseif executable('firefox')
        let g:svg_viewer = 'firefox'
    elseif executable('eog')
        let g:svg_viewer = 'eog'
    endif
elseif g:iswindows
    if executable('chrome')
        let g:svg_viewer = 'chrome'
    elseif executable('firefox')
        let g:svg_viewer = 'firefox'
    endif
endif

" g:png_viewer
if g:islinux
    if executable('google-chrome')
        let g:png_viewer = 'google-chrome'
    elseif executable('firefox')
        let g:png_viewer = 'firefox'
    elseif executable('eog')
        let g:png_viewer = 'eog'
    endif
elseif g:iswindows
    if executable('chrome')
        let g:png_viewer = 'chrome'
    elseif executable('firefox')
        let g:png_viewer = 'firefox'
    endif
endif

" }}}

" Markdown configure {{{
function!  commands#markdown_pre_view() " {{{
    if exists('g:html_view') && executable('pandoc')
        nnoremap <silent><buffer> <leader>mm
\       :call system("pandoc ".expand("%:p")." > ".expand("%:r").
\       ".html && ".g:html_view." ".expand("%:r").".html")<cr>
    endif
endfunction "}}}

function! commands#markdown_pre_view_html_clean() " {{{
    if filereadable(expand("%:r").".html") && exists('g:rm_tool')
        call system(g:rm_tool." ".expand("%:r").".html")
    endif
endfunction " }}}

autocmd FileType markdown call commands#markdown_pre_view()
autocmd BufUnload,BufLeave *.md,*MD call commands#markdown_pre_view_html_clean()
" }}}

" HTML configure {{{

function! commands#html_pre_view() " {{{
    if exists('g:html_view')
        nnoremap <silent><buffer> <leader>mm
\       :call system(g:html_view." ".expand("%:p"))<cr>
    endif
endfunction "}}}

autocmd FileType html call commands#html_pre_view()
" }}}

"{{{ cmake filetype
autocmd! BufReadPost CMakeLists.txt set filetype=cmake
"}}}

" Metapost compile shortcut, and preview {{{

"" Linux and Windows metapost viewer function {{{

function! commands#linux_view_metapost_out() " {{{
    if exists('g:eps_viewer')
        call system("matchFile=`find -regex .*\.eps -cmin -1 -newer ".expand('%:p')."`; for outfile in ${matchFile[@]}; do ".g:eps_viewer." ${outfile} & done")
    endif
    if exists('g:svg_viewer')
        call system("matchFile=`find -regex .*\.svg -cmin -1 -newer ".expand('%:p')."`; for outfile in ${matchFile[@]}; do ".g:svg_viewer." ${outfile} & done")
    endif
    if exists('g:png_viewer')
        call system("matchFile=`find -regex .*\.png -cmin -1 -newer ".expand('%:p')."`; for outfile in ${matchFile[@]}; do ".g:png_viewer." ${outfile} & done")
    endif
endfunction " }}}

function! commands#Windows_view_metapost_out() " {{{
    if filereadable(expand('%:r').".eps") && exists('g:eps_viewer')
        call system(g:eps_viewer." ".expand('%:r').".eps")
    elseif filereadable(expand('%:r').".svg") && exists('g:svg_viewer')
        call system(g:svg_viewer." ".expand('%:r').".svg")
    elseif filereadable(expand('%:r').".png") && exists('g:png_viewer')
        call system(g:png_viewer." ".expand('%:r').".png")
    endif
endfunction " }}}

"" }}}

function! commands#view_metapost_out()  "{{{
    if g:iswindows
        call commands#Windows_view_metapost_out()
    elseif g:islinux
        call commands#linux_view_metapost_out()
    endif
endfunction "}}}

function! commands#run_metapost_map() " {{{
    if executable('mpost')
        nnoremap <silent><buffer> <leader>mm
\       :exe "call system(\"cd \".expand('%:h').\" && mpost \".expand('%:t'))\n
\       call commands#view_metapost_out()"<cr>
    endif
endfunction " }}}

autocmd FileType mp call commands#run_metapost_map()

" }}}

"{{{ Fold

" vimscript fold {{{
function! Foldmethod_Marker() " {{{
  set foldmethod=marker
  set foldmarker={{{,}}}
  return 1
endfunction " }}}
autocmd FileType vim call Foldmethod_Marker()
" }}}

" TeX fold {{{

function! Foldmethod_Marker_TeX() " {{{
  set foldmethod=marker
  set foldmarker=%{{{,%}}}
  return 1
endfunction " }}}

autocmd BufRead *.tex,*.sty call Foldmethod_Marker_TeX()
" }}} End

" Clang fold {{{

function! Foldmethod_Marker_Clang() " {{{
  set foldmethod=marker
  set foldmarker=//{,//}
  return 1
endfunction " }}}

autocmd FileType css,scss,typescript,javascript,c,cpp,asy call Foldmethod_Marker_Clang()
" }}} End

"{{{ powrshell, shell, python, cmake fold

function! Foldmethod_Marker_shell_python() " {{{
  set foldmethod=marker
  set foldmarker=#{,#}
  return 1
endfunction " }}}

autocmd FileType sh,python,ps1,cmake call Foldmethod_Marker_shell_python()
"}}} end shell fold

"{{{ autoLISP and CLISP
function! Foldmethod_Marker_LISP() " {{{
  set foldmethod=marker
  set foldmarker=;{{{,;}}}
  return 1
endfunction " }}}

autocmd FileType lisp call Foldmethod_Marker_LISP()

"}}}

"{{{ Lua
function! Foldmethod_Marker_LUA() " {{{
  set foldmethod=marker
  set foldmarker=--<,-->
  return 1
endfunction " }}}
autocmd FileType lua call Foldmethod_Marker_LUA()
"}}}

"{{{ Markdown
function! Markdown_fold()
    set foldmethod=marker
    set foldmarker=<!--[-->,<!--]-->
    setlocal nofoldenable
endfunction
autocmd Filetype markdown call Markdown_fold()
"}}}

"}}} End fold
