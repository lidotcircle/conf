" Map keys in this part

if has("nvim")
let g:mapleader = ","
else
let g:mapleader = "*"
endif

let g:which_key_map = {
            \ 'name': 'leader',
            \ }

" <ESC> in insert and visual mode [[
inoremap jk <esc>
vnoremap ui <esc>
" ]]

" write buffer and exit [[
nnoremap <silent><leader>q :q<cr>
nnoremap <leader>w :w<cr>
" ]]

let g:which_key_map.v = {
            \ "name": "+ adjust window size",
            \ "k": [":resize -10", "Decrease view's height"],
            \ "j": [":resize +10", "Increase view's height"],
            \ "l": [":vertical resize -10", "Decrease view's width"],
            \ "h": [":vertical resize +10", "Increase view's width"],
            \ }

let g:which_key_map.c = {
            \ "name": "+ change focus",
            \ "j": [":wincmd j", "focus into below"],
            \ "k": [":wincmd k", "focus into upside"],
            \ "h": [":wincmd h", "focus into left"],
            \ "l": [":wincmd l", "focus into right"],
            \ "t": [":wincmd t", "focus into top"],
            \ "b": [":wincmd b", "focus into bottom"],
            \ }

let g:which_key_map.e = {
            \ "name": "+ move view",
            \ "j": [":wincmd J", "move into below"],
            \ "k": [":wincmd K", "move into upside"],
            \ "h": [":wincmd H", "move into left"],
            \ "l": [":wincmd L", "move into right"],
            \ "r": [":wincmd r", "roate view downward or rightward"],
            \ "R": [":wincmd R", "roate view upward or leftward"],
            \ "x": [":wincmd x", "move into lat"],
            \ "o": [":only",     "close other view"],
            \ }

" Copy, cut, paste [[
vnoremap <c-c> "+y
nnoremap <c-v> "+p
vnoremap <c-x> "+c
let g:which_key_map.p=[':setlocal paste!', "toggle paste mode"]
" ]]

let g:which_key_map.b = {
            \ 'name' : '+buffer' ,
            \ '1': [':b1',        'buffer 1'],
            \ '2': [':b2',        'buffer 2'],
            \ '3': [':b3',        'buffer 3'],
            \ '4': [':b4',        'buffer 4'],
            \ '5': [':b5',        'buffer 5'],
            \ 'l': [':blast',     'last-buffer'],
            \ 'n': [':bnext',     'next-buffer'],
            \ 'p': [':bprevious', 'previous-buffer'],
            \ '?': [':buffers',   'list-buffer'],
            \ }

" ReadLine in insert mode [[
inoremap <silent><c-f> <right>
inoremap <silent><c-b> <left>
" inoremap <silent><c-n> <down>
" inoremap <silent><c-p> <up>
inoremap <silent><c-d> <backspace>
inoremap <silent><a-f> <esc><right>wi
inoremap <silent><a-b> <esc><right>bi
"]]

nnoremap <silent><leader>i :call setreg('/', '')<cr>
" let g:which_key_map.i = [":call setreg('/', '')", "clear search"]

function! NextError() "[[
    let l:qflen = len(getqflist())
    if  l:qflen == 0
        echom "quickfix list is empty"
    elseif l:qflen == 1
        cc 1
    else
        try
            cnext
        catch /.*/
            cc 1
        endtry
    endif
endfunction "]]
function! PrevError() "[[
    let l:qflen = len(getqflist())
    if  l:qflen == 0
        echom "quickfix list is empty"
    elseif l:qflen == 1
        cc 1
    else
        try
            cprev
        catch /.*/
            exec "cc " . string(l:qflen)
        endtry
    endif
endfunction "]]
let g:which_key_map.e.n  = [':call NextError()', 'next-error']
let g:which_key_map.e.p  = [':call PrevError()', 'previous-error']


function! CNextError() "[[
    let l:cqflen = len(getloclist(0))
    if  l:cqflen == 0
        echom "current window quickfix list is empty"
    elseif l:cqflen == 1
        ll 1
    else
        try
            lnext
        catch /.*/
            ll 1
        endtry
    endif
endfunction "]]
function! CPrevError() "[[
    let l:cqflen = len(getloclist(0))
    if  l:cqflen == 0
        echom "current window quickfix list is empty"
    elseif l:cqflen == 1
        ll 1
    else
        try
            lprev
        catch /.*/
            exec "ll " . string(l:cqflen)
        endtry
    endif
endfunction "]]
let g:which_key_map.s = {
            \ 'name' : '+ error(current)' ,
            \ 'n': [':call CNextError()', 'next-error'],
            \ 'p': [':call CPrevError()', 'previous-error'],
            \ 's': [':syntax sync fromstart', 'sync syntax highlight'],
            \ }

" termdebug "[[
nnoremap <silent><F5>    :Run<cr>
nnoremap <silent><F9>    :Break<cr>
nnoremap <silent><C-F9>  :Delete<cr>
nnoremap <silent><F10>   :Step<cr>
nnoremap <silent><F11>   :Over<cr>
nnoremap <silent><C-F11> :Finish<cr>
nnoremap <silent><C-F5>  :Continue<cr>
nnoremap <silent><S-F5>  :Stop<cr>
"]]

" tab management [[
nnoremap <silent>g1 1gt
nnoremap <silent>g2 2gt
nnoremap <silent>g3 3gt
nnoremap <silent>g4 4gt
nnoremap <silent>g5 5gt
nnoremap <silent>g6 6gt
nnoremap <silent>g7 7gt
nnoremap <silent>g8 8gt
nnoremap <silent>g9 9gt
" ]] 

