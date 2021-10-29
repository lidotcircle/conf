
call plug#begin('~/.vim/bundle')

let s:plugins= [
            \ [ 'liuchengxu/vim-which-key' ],
            \ [ 'tjdevries/nlua.nvim',    'has("nvim")' ],
            \ [ 'hrsh7th/nvim-cmp',       'has("nvim")' ],
            \ [ 'neovim/nvim-lspconfig',  'has("nvim")' ],
            \ [ 'ycm-core/YouCompleteMe', 'has("python3") || has("python")'],
            \ [ 'williamboman/nvim-lsp-installer', 'has("nvim")' ],
            \
            \ [ 'nvim-lua/popup.nvim',           'has("nvim")' ],
            \ [ 'nvim-lua/plenary.nvim',         'has("nvim")' ],
            \ [ 'nvim-lua/lsp-status.nvim',      'has("nvim")' ],
            \ [ 'nvim-telescope/telescope.nvim', 'has("nvim")' ],
            \ [ 'lidotcircle/nvim-repl',         'has("nvim")' ],
            \
            \ [ 'github/copilot.vim', 'has("nvim")' ],
            \
            \ [ 'vim-airline/vim-airline' ],
            \ [ 'vim-airline/vim-airline-themes' ],
            \ [ 'lfv89/vim-interestingwords' ],
            \ [ 'scrooloose/nerdtree' ],
            \ [ 'fholgado/minibufexpl.vim' ],
            \ [ 'arzg/vim-colors-xcode', 'v:false'],
            \
            \ [ 'itchyny/calendar.vim' ],
            \ [ 'mbbill/undotree' ],
            \ [ 'airblade/vim-gitgutter' ],
            \ [ 'mzlogin/vim-markdown-toc' ],
            \
            \ [ 'mattn/emmet-vim' ],
            \ [ 'othree/csscomplete.vim' ],
            \ [ 'SirVer/ultisnips' ],
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
            \ "self-plugins/win-buffer-only"
            \ ]

for plugpath in map(plugins_list, "expand(loadpath . '/' . v:val)")
    exec "set rtp+=".plugpath
endfor

