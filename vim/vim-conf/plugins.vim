
call plug#begin('~/.vim/bundle')

let s:plugins= [
            \ [ 'liuchengxu/vim-which-key' ],
            \ [ 'neovim/nvim-lspconfig',  'has("nvim")' ],
            \ [ 'ycm-core/YouCompleteMe', 'has("python3") || has("python")'],
            \
            \ [ 'Lokaltog/vim-powerline' ],
            \ [ 'scrooloose/nerdtree' ],
            \ [ 'fholgado/minibufexpl.vim' ],
            \ [ 'othree/csscomplete.vim' ],
            \
            \ [ 'mattn/emmet-vim' ],
            \ [ 'SirVer/ultisnips' ],
            \ [ 'honza/vim-snippets' ],
            \ [ 'mileszs/ack.vim' ],
            \ [ 'junegunn/vim-easy-align' ],
            \
            \ [ 'PProvost/vim-ps1' ],
            \ [ 'leafgarland/typescript-vim' ],
            \ [ 'pboettch/vim-cmake-syntax' ],
            \ [ 'cakebaker/scss-syntax.vim' ],
            \ [ 'digitaltoad/vim-pug' ],
            \ ]

function! s:plugDoAutocmd(plugin)
    let l:v = match(a:plugin, "\/[^/]*$")
    execute "doautocmd User ".a:plugin[l:v+1:]."-loaded"
endfunction
let s:installedPlugins = []

for plugin in s:plugins
    let name = plugin[0]
    if len(plugin) == 1
        Plug name
        call add(s:installedPlugins, name)
    elseif len(plugin) >= 2
        let condition = plugin[1]
        if len(condition)==0 || eval(condition)
            if len(plugin) == 2
                Plug name
            else
                Plug name plugin[2]
            endif
            call add(s:installedPlugins, name)
        endif
    endif
endfor

call plug#end()
for plugin in s:installedPlugins
    call s:plugDoAutocmd(plugin)
endfor


let plugins_list = [
            \ "$LOADPATH/self-plugins/win-buffer-only"
            \ ]

for plugpath in map(plugins_list, "expand(v:val)")
    exec "set rtp+=".plugpath
endfor

