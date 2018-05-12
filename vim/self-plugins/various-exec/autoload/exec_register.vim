" Description: function for executing expression

" Functions {{{
function! exec_register#line_list_register(register_var) "{{{
    if (type(a:register_var) != 1) || (strlen(a:register_var) != 1)
        throw "parameter Error!"
    endif
    let Str_pool = getreg(a:register_var, 'c')
    let Str_list = []
    let pool_ptr_a = 0
    let pool_ptr_b = stridx(Str_pool, nr2char(10))
    while (pool_ptr_b != -1)
        let str_temp = strpart(Str_pool, pool_ptr_a, pool_ptr_b - pool_ptr_a)
        let pool_ptr_a = pool_ptr_b + 1
        let pool_ptr_b = stridx(Str_pool, nr2char(10), pool_ptr_a)
        if (str_temp != "")
            let Str_list = add(Str_list, str_temp)
        endif
    endwhile
    if(strpart(Str_pool, strlen(Str_pool) - 1, 1) != nr2char(10))
        let str_temp = strpart(Str_pool, pool_ptr_a)
        let Str_list = add(Str_list, str_temp)
    endif
    return Str_list
endfunction "}}}

function! exec_register#execute_line_list(line_list) "{{{
    if (type(a:line_list) != 3)
        throw "parameter Error! need a List"
    endif
    let temp_file = tempname()
    call writefile(a:line_list, temp_file)
    exec "source " . expand(temp_file)
    call delete(temp_file)
endfunction "}}}

function! exec_register#execute_register(reg_var) "{{{
    let line_list = exec_register#line_list_register(a:reg_var)
    call exec_register#execute_line_list(line_list)
endfunction "}}}

function! exec_register#execute_v_register() "{{{
    call exec_register#execute_register(v:register)
endfunction "}}}
"}}}
