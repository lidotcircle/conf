local mgr = require("hula.plugins.manager")
local use = mgr.use
local lsp_on_attach = require("hula.plugins.lsp_on_attach")


use { "nvimdev/dashboard-nvim", config = function() require("dashboard").setup() end }
use { 'folke/which-key.nvim', config = function() require("which-key").setup() end }
use {
    'numToStr/Comment.nvim',
    config = function()
        require('Comment').setup({
            ignore = '^$',
            toggler = {
                line = '<leader>cc',
                block = '<leader>bc',
            },
            opleader = {
                line = '<leader>c',
                block = '<leader>b',
            },
        })
    end
}
use 'tjdevries/nlua.nvim'
use { 'folke/neodev.nvim', config = function() require("neodev").setup() end }
use { 'neovim/nvim-lspconfig', config = function()
    local opts = { noremap = true, silent = true }
    vim.api.nvim_set_keymap('n', '<space>e', '<cmd>lua vim.diagnostic.open_float()<CR>', opts)
    vim.api.nvim_set_keymap('n', '[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>', opts)
    vim.api.nvim_set_keymap('n', ']d', '<cmd>lua vim.diagnostic.goto_next()<CR>', opts)
    vim.api.nvim_set_keymap('n', '<space>q', '<cmd>lua vim.diagnostic.setloclist()<CR>', opts)
end
}
use 'hrsh7th/cmp-nvim-lsp'
use 'hrsh7th/cmp-buffer'
use 'hrsh7th/cmp-path'
use 'hrsh7th/cmp-cmdline'
use { 'hrsh7th/nvim-cmp', config = function()
    local cmp = require 'cmp'
    cmp.setup({
        snippet = {
            expand = function(args)
                vim.fn["UltiSnips#Anon"](args.body)
            end,
        },
        mapping = {
            ['<C-d>'] = cmp.mapping(cmp.mapping.scroll_docs(-4), { 'i', 'c' }),
            ['<C-f>'] = cmp.mapping(cmp.mapping.scroll_docs(4), { 'i', 'c' }),
            ['<C-Space>'] = cmp.mapping(cmp.mapping.complete(), { 'i', 'c' }),
            ['<C-y>'] = cmp.config.disable,
            ['<C-e>'] = cmp.mapping({
                i = cmp.mapping.abort(),
                c = cmp.mapping.close(),
            }),
            ['<CR>'] = cmp.mapping.confirm({ select = true }),
            ['<C-n>'] = cmp.mapping(cmp.mapping.select_next_item(), { 'i', 'c' }),
            ['<C-p>'] = cmp.mapping(cmp.mapping.select_prev_item(), { 'i', 'c' }),
        },
        sources = cmp.config.sources({
            { name = 'nvim_lsp' },
            { name = 'ultisnips' },
        }, {
            { name = 'buffer' },
        })
    })
end
}
use 'sakhnik/nvim-gdb'
use 'mfussenegger/nvim-dap'
use {
    'NeogitOrg/neogit',
    config = function() require("neogit").setup() end
}
use {
    'williamboman/mason.nvim',
    config = function() require("mason").setup() end
}
use {
    'williamboman/mason-lspconfig.nvim',
    config = function()
        require("mason-lspconfig").setup()
        require("mason-lspconfig").setup_handlers {
            function(server_name) -- default handler (optional)
                require("lspconfig")[server_name].setup {
                    on_attach = lsp_on_attach,
                    flags = {
                        debounce_text_changes = 150,
                    }
                }
            end,
            ["lua_ls"] = function()
                require("lspconfig").lua_ls.setup {
                    on_attach = lsp_on_attach,
                    flags = {
                        debounce_text_changes = 150,
                    }
                }
            end
        }
    end
}
use 'tanvirtin/monokai.nvim'
use 'mfussenegger/nvim-dap-python'
use {
    'mfussenegger/nvim-dap',
    config = function()
        local wk = require("which-key")
        wk.register({
            d = {
                a = { function() require("dap").continue() end, "DAP Debug" }
            }
        }, { prefix = "<leader>" })
    end
}
use {
    'leoluz/nvim-dap-go',
    config = function()
        require('dap-go').setup {
            dap_configurations = {
                {
                    type = "go",
                    name = "Attach remote",
                    mode = "remote",
                    request = "attach",
                },
            },
            delve = {
                path = "dlv",
                initialize_timeout_sec = 20,
                port = "${port}",
                args = {},
                build_flags = "",
            },
        }
    end
}
use 'rcarriga/nvim-dap-ui'
use {
    'nvim-treesitter/nvim-treesitter',
    config = function()
        require 'nvim-treesitter.configs'.setup {
            ensure_installed = { "c", "cpp", "python", "lua", "vim" },
            sync_install = false,
            auto_install = true,
            ignore_install = {},
            highlight = {
                enable = true,
                disable = function(lang, buf)
                    local max_filesize = 100 * 1024 -- 100 KB
                    local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
                    if lang and ok and stats and stats.size > max_filesize then
                        return true
                    end
                end,
                additional_vim_regex_highlighting = false,
            },
        }
    end
}
use 'nvim-lua/popup.nvim'
use 'nvim-lua/plenary.nvim'
use 'nvim-lua/lsp-status.nvim'
use 'nvim-telescope/telescope.nvim'
use 'smartpde/telescope-recent-files'
use 'lidotcircle/nvim-repl'
use {
    'simrat39/symbols-outline.nvim',
    config = function() require("symbols-outline").setup() end
}
use 'kyazdani42/nvim-web-devicons'
use 'folke/trouble.nvim'
use 'f-person/git-blame.nvim'
use 'sindrets/diffview.nvim'
use {
    'github/copilot.vim',
    config = function()
        vim.g.copilot_no_tab_map = true
        vim.api.nvim_set_keymap("i", "<C-J>", 'copilot#Accept("<CR>")', { silent = true, expr = true })
    end
}
use 'NTBBloodbath/galaxyline.nvim'
-- use 'romgrk/barbar.nvim'
use 'nanozuki/tabby.nvim'
use 'lewis6991/gitsigns.nvim'
use 'nvim-tree/nvim-tree.lua'
use 'andythigpen/nvim-coverage'
use {
    'Shatur/neovim-session-manager',
    config = function()
        local Path = require('plenary.path')
        require('session_manager').setup({
            sessions_dir = Path:new(vim.fn.stdpath('data'), 'sessions'),
            path_replacer = '__',
            colon_replacer = '++',
            autoload_mode = require('session_manager.config').AutoloadMode.Disabled,
            autosave_last_session = true,
            autosave_ignore_not_normal = true,
            autosave_ignore_filetypes = {
                'gitcommit',
            },
            autosave_only_in_session = false,
            max_path_length = 80,
        })
    end
}
use 'quangnguyen30192/cmp-nvim-ultisnips'
use 'gennaro-tedesco/nvim-peekup'

return mgr
