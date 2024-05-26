
call plug#begin('~/.vim/bundle')

let s:plugins= [
            \ [ 'liuchengxu/vim-which-key', '!has("nvim")' ],
            \ [ 'folke/which-key.nvim',     'has("nvim")' ],
            \ [ 'numToStr/Comment.nvim',    'has("nvim")' ],
            \ [ 'tjdevries/nlua.nvim',    'has("nvim")' ],
            \ [ 'folke/neodev.nvim',      'has("nvim")' ],
            \ [ 'neovim/nvim-lspconfig',  'has("nvim")' ],
            \ [ 'hrsh7th/cmp-nvim-lsp', 'has("nvim")' ],
            \ [ 'hrsh7th/cmp-buffer',   'has("nvim")' ],
            \ [ 'hrsh7th/cmp-path',     'has("nvim")' ],
            \ [ 'hrsh7th/cmp-cmdline',  'has("nvim")' ],
            \ [ 'hrsh7th/nvim-cmp',     'has("nvim")' ],
            \ [ 'sakhnik/nvim-gdb',     'has("nvim")' ],
            \ [ 'mfussenegger/nvim-dap', 'has("nvim")' ],
            \ [ 'NeogitOrg/neogit', 'has("nvim")' ],
            \ [ 'ycm-core/YouCompleteMe', '!has("nvim") && (has("python3") || has("python"))'],
            \ [ 'williamboman/nvim-lsp-installer', 'has("nvim")' ],
            \ [ 'tanvirtin/monokai.nvim', 'has("nvim")' ],
            \
            \ [ 'mfussenegger/nvim-dap-python', 'has("nvim")'],
            \ [ 'mfussenegger/nvim-dap', 'has("nvim")'],
            \ [ 'leoluz/nvim-dap-go', 'has("nvim")'],
            \ [ 'rcarriga/nvim-dap-ui', 'has("nvim")'],
            \ [ 'nvim-treesitter/nvim-treesitter', 'has("nvim")'],
            \
            \ [ 'nvim-lua/popup.nvim',           'has("nvim")' ],
            \ [ 'nvim-lua/plenary.nvim',         'has("nvim")' ],
            \ [ 'nvim-lua/lsp-status.nvim',      'has("nvim")' ],
            \ [ 'nvim-telescope/telescope.nvim', 'has("nvim")' ],
            \ [ 'smartpde/telescope-recent-files', 'has("nvim")' ],
            \ [ 'lidotcircle/nvim-repl',         'has("nvim")' ],
            \ [ 'simrat39/symbols-outline.nvim', 'has("nvim")' ],
            \
            \ [ 'kyazdani42/nvim-web-devicons',    'has("nvim")' ],
            \ [ 'folke/trouble.nvim',              'has("nvim")' ],
            \ [ 'f-person/git-blame.nvim',         'has("nvim")' ],
            \ [ 'sindrets/diffview.nvim',          'has("nvim")' ],
            \ [ 'github/copilot.vim' ],
            \
            \ [ 'vim-airline/vim-airline' ],
            \ [ 'romgrk/barbar.nvim', 'has("nvim") &&  v:false' ],
            \ [ 'nanozuki/tabby.nvim', 'has("nvim")' ],
            \ [ 'lewis6991/gitsigns.nvim', 'has("nvim")' ],
            \ [ 'vim-airline/vim-airline-themes' ],
            \ [ 'lfv89/vim-interestingwords' ],
            \ [ 'nvim-tree/nvim-tree.lua', 'has("nvim")' ],
            \ [ 'scrooloose/nerdtree', '!has("nvim")' ],
            \ [ 'fholgado/minibufexpl.vim', '!has("nvim")' ],
            \ [ 'arzg/vim-colors-xcode', 'v:false'],
            \ [ 'andythigpen/nvim-coverage', 'has("nvim")' ],
            \
            \ [ 'itchyny/calendar.vim' ],
            \ [ 'mbbill/undotree' ],
            \ [ 'airblade/vim-gitgutter', '!has("nvim")' ],
            \ [ 'mzlogin/vim-markdown-toc' ],
            \ [ 'xolox/vim-session', '!has("nvim")' ],
            \ [ 'xolox/vim-misc', '!has("nvim")' ],
            \ [ 'Shatur/neovim-session-manager', 'has("nvim")' ],
            \
            \ [ 'mattn/emmet-vim' ],
            \ [ 'voldikss/vim-translator' ],
            \ [ 'othree/csscomplete.vim' ],
            \ [ 'SirVer/ultisnips', 'has("python3")' ],
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

if has("nvim")
    Plug 'michaelb/sniprun', {'do': 'bash install.sh'}
    Plug 'gennaro-tedesco/nvim-peekup'
endif

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

