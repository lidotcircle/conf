" autocmd

" cmake filetype
autocmd! BufReadPost CMakeLists.txt set filetype=cmake

let s:fileTypeFolds = {
            \ "vim":                   "[[,]]",
            \ "tex,sty":               "%{,%}",
            \ "css,scss,,asy":         "//{,//}",
            \ "typescript,javascript": "//{,//}",
            \ "c,cpp,java":            "//{,//}",
            \ "sh,python,ps1,cmake":   "#{,#}",
            \ "lisp":                  "{,}",
            \ "lua":                   "--<,-->",
            \ "markdown":              "<!--[-->,<!--]-->",
            \ }

for [ ftype, marker ] in items(s:fileTypeFolds)
    execute 'autocmd FileType '.ftype.' set foldmethod=marker'
    execute 'autocmd FileType '.ftype.' set foldmarker='.marker
endfor

