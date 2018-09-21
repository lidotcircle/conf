" FileName: asyMain.vim
" Descrption: For compiling asy file in background

highlight AsyError ctermbg=red guibg=red

" functions for dealing errors
function! asyMain#syntaxError(mes) "{{{
    if match(a:mes, '^.*:[[:space:]]*[0-9]*\.[0-9]*:.*$') != -1 " line error
        let l:file = substitute(a:mes, '^\([^:]*\):.*$', '\1', "")
        let b:file_buf = bufnr(l:file)
        let l:errMes = substitute(a:mes, '^.*:[[:space:]]*[0-9]*\.[0-9]*:\(.*$\)', '\1', "")
        let l:line_col = substitute(a:mes, '^[^:]*:\(.*\):.*$', '\1', "")
        let l:line = str2nr(substitute(l:line_col, '^\([0-9]*\)\..*$', '\1', ""))
        let l:col = str2nr(substitute(l:line_col, '^.*\.\([0-9]*\)$', '\1', ""))
        let l:listEnt = {'bufnr': b:file_buf, 'lnum': l:line, 'col': l:col, 'type': 'E', 'text': l:errMes}
        call setqflist([l:listEnt], 'a')
        let b:AsyErrorList = add(b:AsyErrorList, l:listEnt)
    endif
endfunction "}}}
function! asyMain#moduleError(mes) "{{{
    if match(a:mes, '^[[:space:]]*error.*module.*$') != -1 " line error
        let l:errMes = substitute(a:mes, '^[[:space:]]*error:[[:space:]]*\(.*$\)', '\1', "")
        let l:asy_module = substitute(l:errMes, '.*module[[:space:]]*[[:punct:]]\(.*\)[[:punct:]].*',
                    \ '\1', "")
        let b:compiled_buf = getbufline(bufnr(b:compile_file), 1, "$")
        let i = 0
        let l:lnum = 0
        let l:module_pattern = '.*\(import\|from\).*'.l:asy_module.'.*'
        for line_cont in b:compiled_buf
            let i = i + 1
            let mat_ret = match(line_cont, l:module_pattern)
            if mat_ret != -1
                let l:lnum = i
                let l:col = match(line_cont, l:asy_module)
                let l:col = l:col + 1
                echom l:col
                break
            endif
        endfor
        if l:lnum == 0
            let l:listEnt = {'bufnr': 0, 'lnum': 1, 'col': 1, 'type': 'E', 'text': l:errMes}
        else
            let l:listEnt = {'bufnr': bufnr(b:compile_file), 'lnum': l:lnum, 
                        \'col': l:col, 'type': 'E', 'text': l:errMes}
        endif
        call setqflist([l:listEnt], 'a')
"        let b:AsyErrorList = add(b:AsyErrorList, l:listEnt)
    endif
endfunction "}}}

function! AsyErrorCB(pipe, mes) "{{{
    call asyMain#syntaxError(a:mes)
    call asyMain#moduleError(a:mes)
    return
endfunction "}}}

function! AsyExitCB(jobId, exit_status) "{{{
    if a:exit_status != 0
        echom "Compiling this file failed."
    else
        echom "Compiling this file success."
    endif
    return
endfunction "}}}

function! asyMain#firstCompile(filename) "{{{
    let buf_name = bufname(a:filename)
    if getbufline(buf_name, 1)[0] == "// root"
        let b:asy_job = job_start(["asy", "-V", "-debug", a:filename],
                    \{"err_cb":'AsyErrorCB', "exit_cb": 'AsyExitCB'}
                    \)
        echom b:asy_job." has been created to compile <".a:filename."> ."
        let b:already_start_viewer = 1
        return b:asy_job
    else
        return 1
    endif
endfunction "}}}

function! asyMain#restCompile(filename) "{{{
    let buf_name = bufname(a:filename)
    if exists("b:already_start_viewer") 
        if (getbufline(buf_name, 1)[0] == "// root")
            let b:asy_job = job_start(["asy", "-noV", "-debug", a:filename],
                        \{"err_cb":'AsyErrorCB', "exit_cb": 'AsyExitCB'}
                        \)
            echom b:asy_job." has been created to compile <".a:filename."> ."
            return b:asy_job
        else
            return 1
        endif
    else
        throw "calling function: asyMain#restCompile() is erroneous"
    endif
endfunction "}}}

function! asyMain#Compile(filename) "{{{
    let b:compile_result = 1 " default is fail, because asymptote may stuck in invoking tex
    if exists("b:cjob_id") && type(b:cjob_id) && job_status(b:cjob_id) == "run"
        echom "previous process of compiling <".a:filename."> still running, please wait!!!"
    endif
    let b:AsyErrorList = [] " error list
    let b:compile_file = a:filename
    call setqflist([]) " clean quickfix list
    if exists("b:already_start_viewer")
        let b:cjob_id = asyMain#restCompile(a:filename)
    else
        let b:cjob_id = asyMain#firstCompile(a:filename)
    endif
endfunction "}}}
