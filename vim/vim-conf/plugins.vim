
call plug#begin('~/.vim/bundle')

let s:plugins= [
            \ [ 'liuchengxu/vim-which-key' ],
            \ [ 'tjdevries/nlua.nvim',    'has("nvim")' ],
            \ [ 'neovim/nvim-lspconfig',  'has("nvim")' ],
            \ [ 'hrsh7th/cmp-nvim-lsp', 'has("nvim")' ],
            \ [ 'hrsh7th/cmp-buffer',   'has("nvim")' ],
            \ [ 'hrsh7th/cmp-path',     'has("nvim")' ],
            \ [ 'hrsh7th/cmp-cmdline',  'has("nvim")' ],
            \ [ 'hrsh7th/nvim-cmp',     'has("nvim")' ],
            \ [ 'sakhnik/nvim-gdb',     'has("nvim")' ],
            \ [ 'ycm-core/YouCompleteMe', '!has("nvim") && (has("python3") || has("python"))'],
            \ [ 'williamboman/nvim-lsp-installer', 'has("nvim")' ],
            \
            \ [ 'nvim-lua/popup.nvim',           'has("nvim")' ],
            \ [ 'nvim-lua/plenary.nvim',         'has("nvim")' ],
            \ [ 'nvim-lua/lsp-status.nvim',      'has("nvim")' ],
            \ [ 'nvim-telescope/telescope.nvim', 'has("nvim")' ],
            \ [ 'lidotcircle/nvim-repl',         'has("nvim")' ],
            \
            \ [ 'kyazdani42/nvim-web-devicons',    'has("nvim")' ],
            \ [ 'folke/trouble.nvim',              'has("nvim")' ],
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
            \ [ 'voldikss/vim-translator' ],
            \ [ 'othree/csscomplete.vim' ],
            \ [ 'SirVer/ultisnips' ],
            \ [ 'quangnguyen30192/cmp-nvim-ultisnips', 'has("nvim")' ],
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

if has("nvim")
lua <<EOF
local function setup_vim_plug(plug)
    if (_G['plugcallbacks'] and
        _G['plugcallbacks'][plug] and
        type(_G['plugcallbacks'][plug]) == 'function') then
        _G['plugcallbacks'][plug]()
    else
        _G['loaded_but_not_setup_plugs'] = _G['loaded_but_not_setup_plugs'] or {}
        _G['loaded_but_not_setup_plugs'][plug] = true
    end
end
_G['setup_vim_plug'] = setup_vim_plug
EOF
endif

function! s:trigger_lua_plugin_load_callback(plug)
    if !has("nvim")
        return
    endif

    call luaeval("setup_vim_plug(_A)", a:plug)
endfunction

function! s:plugDoAutocmd(plugin)
    let l:v = match(a:plugin, "\/[^/]*$")
    let basename = a:plugin[l:v+1:]
    let l:santiBasename = substitute(basename, '-\|\.', "_", "g")
    execute "let g:is_".l:santiBasename."_loaed = v:true"
    augroup DummyAutocmd
        execute "autocmd! User ".basename."-loaded normal ''"
    augroup end
    execute "doautocmd User ".basename."-loaded"
    call s:trigger_lua_plugin_load_callback(basename)
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

