" FileName: toc-markdown.vim
" Descrption: generate Table of contents for markdown title

if(&filetype != 'markdown') && !(has('g:maybeMD'))
    finish
endif

let g:maybeMD = 1

function! s:genTitleList(start, end) "{{{ 由(Start, End)获取文件行List
    try
        let a:count = a:start
        let a:TitleList = []
        let a:addFlags = 0
        while (a:count <= a:end)
            let a:line_temp = getline(a:count)
            let a:count = a:count + 1
            if (a:line_temp =~ '[`]\{3\}.*')
                let a:addFlags = a:addFlags + 1
            endif
            if (a:line_temp =~ '^[#]\{1,5\}.*') && (a:addFlags%2 == 0)
                let a:TitleList = add(a:TitleList, a:line_temp)
            endif
        endwhile
    catch /^.*$/
       techo "Exception rise! catch " . v:exception . " in " . v:throwpoint . "."
    endtry
        return a:TitleList
endfunction "}}}

function! s:mapTitleListLevel(key, val) "{{{ 得到List的title级别
    if (a:val =~ '^[#]\{1\} .*')
        return 1
    elseif (a:val =~ '^[#]\{2\} .*')
        return 2
    elseif (a:val =~ '^[#]\{3\} .*')
        return 3
    elseif (a:val =~ '^[#]\{4\} .*')
        return 4
    else
        return 5
    endif
endfunction " }}}

function! s:titleMain(ttlList) "{{{ 重新生成目录的标题
    try
        if (type(a:ttlList) != v:t_list)
            throw "Args: Error"
        endif
    catch
        echo "catch " . v:exception . " in " . v:throwpoint . "."
    endtry
endfunction " }}}

function! s:mapTitleList2Link(key, val) "{{{ 将title映射成链接
    if (type(a:val) != v:t_string)
        let a:temp = string(a:val)
    else
        let a:temp = a:val
    endif
    try
        let a:temp = substitute(a:temp, '^[#]\{1,5\}[[:space:]]*', "", "")
        let a:temp = substitute(a:temp, '[[:punct:]]*', "", "g")
        let a:temp = tr(a:temp, ' ', '-')
        let a:temp = substitute(a:temp, '[-]\{1,\}', '-', "g")
        let a:temp = tolower(a:temp)
    catch
        echo "catch " . v:exception . " in " . v:throwpoint . "."
    endtry
    return ("(#" . a:temp. ")")
endfunction "}}}

function! s:mapTitleList2Title(key, val) "{{{ 将markdown title映射成目录title
    if (type(a:val) != v:t_string)
        let a:temp = string(a:val)
    else
        let a:temp = a:val
    endif
    try
        let a:temp = substitute(a:temp, '^[#]\{1,5\}[[:space:]]*', "", "")
    catch
        echo "catch " . v:exception . " in " . v:throwpoint . "."
    endtry
    return ("* [" . a:temp. "]")
endfunction "}}}

function! s:genFinalList(levelList, titleList, linkList) " {{{ 由三个List生成最后的 List
    let g:maxLevel = min(a:levelList)
    let a:length = len(a:levelList)
    let a:finalIndent = map(a:levelList, {key, val -> val - g:maxLevel})
    let a:count = 0
    let a:result = []
    while (a:count < a:length)
        if (a:finalIndent[a:count] == 0)
            let a:result = add(a:result,  a:titleList[a:count] . a:linkList[a:count])
        elseif (a:finalIndent[a:count] == 1)
            let a:result = add(a:result,  "\t" . a:titleList[a:count] . a:linkList[a:count])
        elseif (a:finalIndent[a:count] == 2)
            let a:result = add(a:result,  "\t\t" . a:titleList[a:count] . a:linkList[a:count])
        elseif (a:finalIndent[a:count] == 3)
            let a:result = add(a:result,  "\t\t\t" . a:titleList[a:count] . a:linkList[a:count])
        elseif (a:finalIndent[a:count] == 4)
            let a:result = add(a:result,  "\t\t\t\t" . a:titleList[a:count] . a:linkList[a:count])
        endif
        let a:count = a:count + 1
    endwhile
    return a:result
endfunction " }}}

function! s:finalFunc() " {{{ 生成最后的List
    let a:lineList = s:genTitleList(1, line('$'))
    let a:levelList = copy(a:lineList)
    let a:titleList = copy(a:lineList)
    let a:linkList = copy(a:lineList)
    let a:levelList = map(a:levelList, function('s:mapTitleListLevel'))
    let a:titleList = map(a:titleList, function('s:mapTitleList2Title'))
    let a:linkList = map(a:linkList, function('s:mapTitleList2Link'))
    let a:resultList = s:genFinalList(a:levelList, a:titleList, a:linkList)
    return a:resultList
endfunction " }}}

function! s:genPas() "{{{ 使用p粘贴目录
    let a:temp = ""
    let a:listfile = s:finalFunc()
    for a:lines in a:listfile
        let a:temp = a:temp . a:lines . "\n"
    endfor
    call setreg(v:register, a:temp, "c")
endfunction "}}}

" GenPas ref to s:genPas()
let GenPas = function('s:genPas', [])

nnoremap <buffer><silent> <leader>gc :call GenPas()<cr>
