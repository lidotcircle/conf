" 插件的快捷键总览 {{{
" |===============================|
" | 快捷键  |       描述          |
" |---------+---------------------|
" |   <F5>  | 刷新ycm的编译文件   |
" |---------+---------------------|
" |<leader>d| NERDTree的界面toggle|
" |---------+---------------------|
" |   <F8>  | tabular的界面toggle |
" |---------+---------------------|
" |  <c-p>  | 调用ctrlp           |
" |===============================|
" }}}

" Mapping part.  {{{
"
" Insert mode中jk代替<esc>,Visual mode中ui代替<esc>
inoremap jk <esc>
vnoremap ui <esc>

" 配置退出
nnoremap <leader>q :q<cr>
nnoremap <leader>w :w<cr>

"Normal mode将..映射到:
nnoremap .. :

" 分隔窗口
"增加窗口高度
nnoremap <leader>vk :resize +1<cr>
"减小窗口高度
nnoremap <leader>vj :resize -1<cr>
"增加窗口宽度
nnoremap <leader>vl :vertical resize +1<cr>
"减小窗口宽度
nnoremap <leader>vh :vertical resize -1<cr>
" :resize [-|+]n                            定义窗口大小
"
"移动光标到下一个窗口
nnoremap <leader>cj :wincmd j<cr>
"移动光标到上一个窗口
nnoremap <leader>ck :wincmd k<cr>
"移动光标到左边的窗口
nnoremap <leader>ch :wincmd h<cr>
"移动光标到右边的窗口
nnoremap <leader>cl :wincmd l<cr>
"移动光标到顶部的窗口
nnoremap <leader>ct :wincmd t<cr>
"移动光标到底部的窗口
nnoremap <leader>cb :wincmd b<cr>
" :[n]wincmd {arg}                          "移动光标
"
"移动窗口到下边全width
nnoremap <leader>ej :wincmd J<cr>
"移动窗口到上边全width
nnoremap <leader>ek :wincmd K<cr>
"移动窗口到左边全heigth
nnoremap <leader>eh :wincmd H<cr>
"移动窗口到右边全heigth
nnoremap <leader>el :wincmd L<cr>
"downward移动窗口
nnoremap <leader>er :wincmd r<cr>
"upward移动窗口
nnoremap <leader>eR :wincmd R<cr>
"移动当前窗口到最后
nnoremap <leader>ex :wincmd x<cr>
"只保留当前一个窗口
nnoremap <leader>eo :only<cr>
" :split
" :vsplit
" :split new<cr>                            新的水平o 'hello'"口
":vsplit new<cr>                           新的垂直窗口
" winnr() return the windows-number of current window
" winnr("$") return the windows-number of last window
" winnr("#") return the windows-number of last access window
" win_getid() return the windows-id of current window
" win_getid(integer) return the windows-id of the integer window
" win_id2tabwin() return the tab/windows-number of the current window
" win_id2tabwin(win-id) return the tab/window-number of the win-id window

" buffer部分
nnoremap <leader>ls :ls<cr>
"后一个buffer
nnoremap <leader>bn :bn<cr>
"前一个buffer
nnoremap <leader>bp :bp<cr>
" bufnr(bufname(string)) return the buffer number of the bufname(string)
" bufnr("$") return the total counter of buffer
" bufnr("#") return the last access buffer
" bufname(integer) return the buffer name of current buffer

" argument list部分
"查看agurment list
nnoremap <leader>la :ar<cr>


" Gvim屏幕模式设置, 需要安装wmctrl在GNU/Linux中
if has('gui_running') && g:islinux
    nnoremap <silent><F11>
\   :call system("wmctrl -ir ".v:windowid." -b toggle,fullscreen")<cr>
endif

" 复制，粘贴，剪切
vnoremap <c-c> "+y
nnoremap <c-v> "+p
vnoremap <c-x> "+c

"   }}}
