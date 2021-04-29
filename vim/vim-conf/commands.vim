" autocmd

" cmake filetype
autocmd! BufReadPost CMakeLists.txt set filetype=cmake

let s:fileTypeFolds = {
            \ "vim":                   "[[,]]",
            \ "tex,sty":               "%{,%}",
            \ "css,scss,asy":         "//{,//}",
            \ "typescript,javascript": "//{,//}",
            \ "c,cpp,java":            "//{,//}",
            \ "bash,sh,python,ps1,cmake":   "#{,#}",
            \ "lisp":                  "{,}",
            \ "lua":                   "--<,-->",
            \ "markdown":              "<!--[-->,<!--]-->",
            \ }

augroup ftfoldmarker
for [ ftype, marker ] in items(s:fileTypeFolds)
    execute 'autocmd! FileType '.ftype.' set foldmethod=marker | set foldmarker='.marker
endfor
augroup end

