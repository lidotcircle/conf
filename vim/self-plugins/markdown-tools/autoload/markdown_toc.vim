" FileName: markdown-toc.vim
" Descrption: generate Table of contents for markdown title

function! markdown_toc#genTitleList(start, end) "{{{ get current file <header> list
    let temp = ""
    try
        let line_count = a:start
        let TitleList = []
        let addFlags = 0
        while (line_count <= a:end)
            let line_temp = getline(line_count)
            let line_count = line_count + 1
            if (line_temp =~ '[`]\{3\}.*')
                let addFlags = addFlags + 1
            endif
            if (line_temp =~ '^[#]\{1,5\}.*') && (addFlags%2 == 0)
                let TitleList = add(TitleList, line_temp)
            endif
        endwhile
    catch /^.*$/
       echo "Exception rise! catch " . v:exception . " in " . v:throwpoint . "."
    endtry
        return TitleList
endfunction "}}}

function! markdown_toc#mapTitleListLevel(key, val) "{{{ get indent level
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

function! markdown_toc#map_mdtitle_to_toclink(key, val) "{{{ map markdown title to toc link
    let temp = ""
    if (type(a:val) != 1)
        let temp = string(a:val)
    else
        let temp = a:val
    endif
    try
        let temp = substitute(temp, '^[#]\{1,5\}[[:space:]]*', "", "")
        let temp = substitute(temp, '[[:punct:]]*', "", "g")
        let temp = tr(temp, ' ', '-')
        let temp = substitute(temp, '[-]\{1,\}', '-', "g")
        let temp = tolower(temp)
    catch
        echo "catch " . v:exception . " in " . v:throwpoint . "."
    endtry
    return ("(#" . temp. ")")
endfunction "}}}

function! markdown_toc#map_mdtitle_to_toctitle(key, val) "{{{ markdown title to toc title
    let temp = ""
    if (type(a:val) != 1)
        let temp = string(a:val)
    else
        let temp = a:val
    endif
    try
        let temp = substitute(temp, '^[#]\{1,5\}[[:space:]]*', "", "")
    catch
        echo "catch " . v:exception . " in " . v:throwpoint . "."
    endtry
    return ("* [" . temp. "]")
endfunction "}}}

function! s:level_map_easy(key, val) "{{{
    return (a:val - g:maxLevel)
endfunction "}}}

function! markdown_toc#gen_final_list_by_three_list(levelList, titleList, linkList) " {{{ combine three list
    let g:maxLevel = min(a:levelList)
    let list_length = len(a:levelList)
    let finalIndent = map(a:levelList, 's:level_map_easy(v:key,v:val)')
    let count_count = 0
    let final_result = []
    while (count_count < list_length)
        if (finalIndent[count_count] == 0)
            let final_result = add(final_result,  a:titleList[count_count] . a:linkList[count_count])
        elseif (finalIndent[count_count] == 1)
            let final_result = add(final_result,  "\t" . a:titleList[count_count] . a:linkList[count_count])
        elseif (finalIndent[count_count] == 2)
            let final_result = add(final_result,  "\t\t" . a:titleList[count_count] . a:linkList[count_count])
        elseif (finalIndent[count_count] == 3)
            let final_result = add(final_result,  "\t\t\t" . a:titleList[count_count] . a:linkList[count_count])
        elseif (finalIndent[count_count] == 4)
            let final_result = add(final_result,  "\t\t\t\t" . a:titleList[count_count] . a:linkList[count_count])
        endif
        let count_count = count_count + 1
    endwhile
    return final_result
endfunction " }}}

function! markdown_toc#final_list() " {{{ generate final List
    let a:lineList = markdown_toc#genTitleList(1, line('$'))
    let a:levelList = copy(a:lineList)
    let a:titleList = copy(a:lineList)
    let a:linkList = copy(a:lineList)
    let a:levelList = map(a:levelList, 'markdown_toc#mapTitleListLevel(v:key, v:val)')
    let a:titleList = map(a:titleList, 'markdown_toc#map_mdtitle_to_toctitle(v:key, v:val)')
    let a:linkList = map(a:linkList, 'markdown_toc#map_mdtitle_to_toclink(v:key, v:val)')
    let a:resultList = markdown_toc#gen_final_list_by_three_list(a:levelList, a:titleList, a:linkList)
    return a:resultList
endfunction " }}}

function! markdown_toc#gen_toc_to_reg(reg_var) "{{{ generate toc to v:register
    let temp = ""
    let listfile = markdown_toc#final_list()
    for lines in listfile
        let temp = temp . lines . "\n"
    endfor
    call setreg(a:reg_var, temp, "c")
endfunction "}}}
