local mgr = require("hula.plugins.manager")
local use = mgr.use
local lsp_on_attach = require("hula.plugins.lsp_on_attach")

local function nnoremap(lhs, rhs)
    vim.api.nvim_set_keymap('n', lhs, rhs, { noremap = true, silent = true })
end
local function nmap(lhs, rhs)
    vim.api.nvim_set_keymap('n', lhs, rhs, { noremap = false, silent = true })
end
local function vmap(lhs, rhs)
    vim.api.nvim_set_keymap('v', lhs, rhs, { noremap = false, silent = true })
end

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
    nnoremap('<space>e', '<cmd>lua vim.diagnostic.open_float()<CR>')
    nnoremap('[d', '<cmd>lua vim.diagnostic.goto_prev()<CR>')
    nnoremap(']d', '<cmd>lua vim.diagnostic.goto_next()<CR>')
    nnoremap('<space>q', '<cmd>lua vim.diagnostic.setloclist()<CR>')
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
use {
    'mfussenegger/nvim-dap',
    config = function()
        nnoremap("<leader>lc", "<Cmd>lua require'dap'.continue()<CR>")
        nnoremap("<leader>ln", "<Cmd>lua require'dap'.step_over()<CR>")
        nnoremap("<leader>ls", "<Cmd>lua require'dap'.step_into()<CR>")
        nnoremap("<leader>lo", "<Cmd>lua require'dap'.step_out()<CR>")
        nnoremap("<Leader>lb", "<Cmd>lua require'dap'.toggle_breakpoint()<CR>")
        nnoremap("<Leader>lB", "<Cmd>lua require'dap'.set_breakpoint(vim.fn.input('Breakpoint condition: '))<CR>")
        nnoremap("<Leader>lp", "<Cmd>lua require'dap'.set_breakpoint(nil, nil, vim.fn.input('Log point message: '))<CR>")
        nnoremap("<Leader>dr", "<Cmd>lua require'dap'.repl.open()<CR>")
        nnoremap("<Leader>dl", "<Cmd>lua require'dap'.run_last()<CR>")
    end
}
use {
    'NeogitOrg/neogit',
    config = function()
        require("neogit").setup()
        nnoremap("<leader>gg", ":Neogit<CR>")
    end
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
use {
    'tanvirtin/monokai.nvim',
    config = function()
        require('monokai').setup { palette = require('monokai').pro }
    end
}
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
use {
    'nvim-telescope/telescope.nvim',
    config = function()
        nnoremap("<leader>fs", "<cmd>Telescope<cr>")
        nnoremap("<leader>ff", "<cmd>Telescope find_files<cr>")
        nnoremap("<leader>fg", "<cmd>Telescope live_grep<cr>")
        nnoremap("<leader>fb", "<cmd>Telescope buffers<cr>")
        nnoremap("<leader>fh", "<cmd>Telescope help_tags<cr>")
    end
}
use {
    'smartpde/telescope-recent-files',
    config = function()
        require('telescope').load_extension("recent_files")
        nnoremap("<Leader><Leader>", "<cmd>lua require('telescope').extensions.recent_files.pick()<CR>")
    end
}
use {
    'lidotcircle/nvim-repl',
    config = function()
        nmap("<leader>ax", "<Plug>(nvim-repl-current-line)")
        nmap("<leader>af", "<Plug>(nvim-repl-current-file)")
        vmap("<silent>aa", "<Plug>(nvim-repl-selection)")
        nmap("<leader>ar", "<Plug>(nvim-repl-reset-interpreter)")
        nmap("<leader>ac", "<Plug>(nvim-repl-win-close)")
        nmap("<leader>ao", "<Plug>(nvim-repl-win-open)")
        nmap("<leader>at", "<Plug>(nvim-repl-win-toggle)")
        nmap("<leader>al", "<Plug>(nvim-repl-buffer-clear)")
        nmap("<leader>as", "<Plug>(nvim-repl-buffer-close)")
        nmap("<leader>am", "<Plug>(nvim-repl-toggle-internal-external-mode)")
        nmap("<leader>ap", "<Plug>(nvim-repl-show-prompt)")
        nmap("<leader>ab", "<Plug>(nvim-repl-show-prompt-bash)")
    end
}
use {
    'simrat39/symbols-outline.nvim',
    config = function()
        require("symbols-outline").setup()
        nnoremap("<leader>so", "<cmd>SymbolsOutline<CR>")
    end
}
use {
    'kyazdani42/nvim-web-devicons',
    config = function()
        require('nvim-web-devicons').setup()
    end
}
use {
    'folke/trouble.nvim',
    config = function()
        nnoremap("<leader>xx", "<cmd>TroubleToggle<cr>")
        nnoremap("<leader>xw", "<cmd>TroubleToggle workspace_diagnostics<cr>")
        nnoremap("<leader>xd", "<cmd>TroubleToggle document_diagnostics<cr>")
        nnoremap("<leader>xq", "<cmd>TroubleToggle quickfix<cr>")
        nnoremap("<leader>xl", "<cmd>TroubleToggle loclist<cr>")
        nnoremap("<leader>gR", "<cmd>TroubleToggle lsp_references<cr>")
    end
}
use 'f-person/git-blame.nvim'
use 'sindrets/diffview.nvim'
use {
    'github/copilot.vim',
    config = function()
        vim.g.copilot_no_tab_map = true
        vim.api.nvim_set_keymap("i", "<C-J>", 'copilot#Accept("<CR>")', { silent = true, expr = true })
    end
}
use {
    'NTBBloodbath/galaxyline.nvim',
    config = function()
        require("galaxyline.themes.eviline")
        require("galaxyline").load_galaxyline()
    end
}
-- use 'romgrk/barbar.nvim'
use { 'nanozuki/tabby.nvim', config = function() require('tabby').setup() end }
use {
    'lewis6991/gitsigns.nvim',
    config = function()
        require('gitsigns').setup()
        nnoremap("[c", "<cmd>Gitsigns prev_hunk<cr>")
        nnoremap("]c", "<cmd>Gitsigns next_hunk<cr>")
        nnoremap("<leader>hp", "<cmd>Gitsigns prev_hunk<cr>")
        nnoremap("<leader>hn", "<cmd>Gitsigns next_hunk<cr>")
        nnoremap("<leader>hq", "<cmd>Gitsigns setloclist<cr>")
        nnoremap("<leader>hs", "<cmd>Gitsigns stage_hunk<cr>")
        nnoremap("<leader>hS", "<cmd>Gitsigns stage_buffer<cr>")
        nnoremap("<leader>hu", "<cmd>Gitsigns reset_hunk<cr>")
        nnoremap("<leader>hv", "<cmd>Gitsigns preview_hunk_inline<cr>")
        nnoremap("<leader>hV", "<cmd>Gitsigns preview_hunk<cr>")
        nnoremap("<leader>hf", "<cmd>Gitsigns toggle_signs<cr>")
        nnoremap("<leader>hd", "<cmd>Gitsigns diffthis<cr>")
    end
}
use {
    'nvim-tree/nvim-tree.lua',
    config = function()
        require('nvim-tree').setup()
        nnoremap("<leader>n", "<cmd>NvimTreeToggle<CR>")
        nnoremap("<leader>fn", "<cmd>NvimTreeFindFile<CR>")
    end
}
use {
    'andythigpen/nvim-coverage',
    config = function()
        require('coverage').setup(
            {
                auto_reload = true,
                lang = { cpp = { coverage_file = 'build/coverage.info' } }
            })
        nnoremap('<leader>cv', '<cmd>CoverageToggle<CR>')
        nnoremap('<leader>cd', '<cmd>CoverageLoad<CR><cmd>CoverageShow<CR>')
    end
}
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
