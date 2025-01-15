
call plug#begin('~/.vim/bundle')

let s:plugins= [
            \ [ 'liuchengxu/vim-which-key', '!has("nvim")' ],
            \ [ 'ycm-core/YouCompleteMe', '!has("nvim") && (has("python3") || has("python"))'],
            \ [ 'vim-airline/vim-airline', '!has("nvim")' ],
            \ [ 'vim-airline/vim-airline-themes' ],
            \ [ 'lfv89/vim-interestingwords' ],
            \ [ 'scrooloose/nerdtree', '!has("nvim")' ],
            \ [ 'fholgado/minibufexpl.vim', '!has("nvim")' ],
            \ [ 'arzg/vim-colors-xcode', 'v:false'],
            \
            \ [ 'itchyny/calendar.vim' ],
            \ [ 'mbbill/undotree' ],
            \ [ 'airblade/vim-gitgutter', '!has("nvim")' ],
            \ [ 'mzlogin/vim-markdown-toc' ],
            \ [ 'xolox/vim-session', '!has("nvim")' ],
            \ [ 'xolox/vim-misc', '!has("nvim")' ],
            \ [ 'Exafunction/codeium.vim' ],
            \
            \ [ 'mattn/emmet-vim' ],
            \ [ 'voldikss/vim-translator' ],
            \ [ 'othree/csscomplete.vim' ],
            \ [ 'SirVer/ultisnips', 'has("python3")' ],
            \ [ 'honza/vim-snippets' ],
            \ [ 'mileszs/ack.vim' ],
            \ [ 'junegunn/vim-easy-align' ],
            \ [ 'rhysd/vim-clang-format' ],
            \
            \ [ 'euclidianAce/BetterLua.vim' ],
            \ [ 'PProvost/vim-ps1' ],
            \ [ 'leafgarland/typescript-vim' ],
            \ [ 'pboettch/vim-cmake-syntax' ],
            \ [ 'cakebaker/scss-syntax.vim' ],
            \ [ 'digitaltoad/vim-pug' ],
            \ ]

let s:installed_plugins = []
function! HulaAddPlugin(name)
    if index(s:installed_plugins, a:name) != -1
        return
    endif
    Plug a:name
    call add(s:installed_plugins, a:name)
endfunction

if has("nvim")
lua <<EOF
require('hula.plugins').AddPluginsWithVimFunction("HulaAddPlugin");
EOF
endif

function! s:plugDoAutocmd(plugin)
    let l:v = match(a:plugin, "\/[^/]*$")
    let basename = a:plugin[l:v+1:]
    let l:santiBasename = substitute(basename, '-\|\.', "_", "g")
    execute "let g:is_".l:santiBasename."_loaed = v:true"
    augroup DummyAutocmd
        execute "autocmd! User ".basename."-loaded normal ''"
    augroup end
    execute "doautocmd User ".basename."-loaded"
endfunction

for plugin in s:plugins
    let name = plugin[0]
    if len(plugin) == 1
        call HulaAddPlugin(name)
    elseif len(plugin) >= 2
        let condition = plugin[1]
        if len(condition)==0 || eval(condition)
            call HulaAddPlugin(name)
        endif
    endif
endfor

call plug#end()
for plugin in s:installed_plugins
    call s:plugDoAutocmd(plugin)
endfor

if has("nvim")
lua <<EOF
    require('hula.plugins').AfterPluginsLoaded()
EOF
endif

let plugins_list = [
            \ "self-plugins/win-buffer-only"
            \ ]

for plugpath in map(plugins_list, "expand(loadpath . '/' . v:val)")
    exec "set rtp+=".plugpath
endfor

