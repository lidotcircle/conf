local mgr = require("hula.plugins.manager")
local use = mgr.use
local lsp_on_attach = require("hula.plugins.lsp_on_attach")

local major = vim.version().major
local minor = vim.version().minor
local has_ultisnips = vim.fn.has("python3") == 1

local function nnoremap(lhs, rhs)
    vim.api.nvim_set_keymap('n', lhs, rhs, { noremap = true, silent = true })
end
local function nmap(lhs, rhs)
    vim.api.nvim_set_keymap('n', lhs, rhs, { noremap = false, silent = true })
end
local function vmap(lhs, rhs)
    vim.api.nvim_set_keymap('v', lhs, rhs, { noremap = false, silent = true })
end

use 'nvim-lua/popup.nvim'
use 'nvim-lua/plenary.nvim'
use { "nvimdev/dashboard-nvim", config = function() require("dashboard").setup() end }
use { "echasnovski/mini.icons" }
use { 'folke/which-key.nvim', config = function() require("which-key").setup() end }
use {
    'folke/sidekick.nvim',
    config = function() require('hula.plugins.ai').setup() end,
}
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
    nnoremap('[d', '<cmd>lua vim.diagnostic.jump({ count = -1, float = true })<CR>')
    nnoremap(']d', '<cmd>lua vim.diagnostic.jump({ count = 1, float = true })<CR>')
    nnoremap('<space>q', '<cmd>lua vim.diagnostic.setloclist()<CR>')

    if major > 0 or minor >= 11 then
        local ls_list = { 'clangd', 'lua_ls', 'ts_ls', 'pylsp', 'cmake' }
        for _, lsp in ipairs(ls_list) do
            vim.lsp.config(lsp, {
                on_attach = lsp_on_attach,
                capabilities = require("cmp_nvim_lsp").default_capabilities(),
            })
        end
    end
end
}
use 'hrsh7th/cmp-nvim-lsp'
use 'hrsh7th/cmp-buffer'
use 'hrsh7th/cmp-path'
use 'hrsh7th/cmp-cmdline'
use { 'hrsh7th/nvim-cmp', config = function()
    local cmp = require 'cmp'
    local sources = { { name = 'nvim_lsp' } }
    if has_ultisnips then
        table.insert(sources, { name = 'ultisnips' })
    end
    cmp.setup({
        snippet = {
            expand = function(args)
                if has_ultisnips then
                    vim.fn["UltiSnips#Anon"](args.body)
                else
                    vim.snippet.expand(args.body)
                end
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
        sources = cmp.config.sources(sources, {
            { name = 'buffer' },
        })
    })
end
}
use {
    'sakhnik/nvim-gdb',
    config = function()
        nnoremap('<leader>bt', '<cmd>GdbBreakpointToggle<CR>')
        vim.api.nvim_create_autocmd({ "FileType" }, {
            callback = function(ev)
                if type(ev.match) == "string" and string.match(ev.match, "nvimgdb") then
                    vim.api.nvim_buf_set_keymap(ev.buf, "n", "<space>n", "<Cmd>GdbNext<CR>",
                        { noremap = true, silent = true })
                    vim.api.nvim_buf_set_keymap(ev.buf, "n", "<space>s", "<Cmd>GdbStep<CR>",
                        { noremap = true, silent = true })
                    vim.api.nvim_buf_set_keymap(ev.buf, "n", "<space>o", "<Cmd>GdbFinish<CR>",
                        { noremap = true, silent = true })
                    vim.api.nvim_buf_set_keymap(ev.buf, "n", "<space>c", "<Cmd>GdbContinue<CR>",
                        { noremap = true, silent = true })
                    vim.api.nvim_buf_set_keymap(ev.buf, "n", "<space>t", "<Cmd>GdbDebugStop<CR>",
                        { noremap = true, silent = true })
                    vim.api.nvim_buf_set_keymap(ev.buf, "n", "<space>p", "<Cmd>GdbInterrupt<CR>",
                        { noremap = true, silent = true })
                    vim.api.nvim_buf_set_keymap(ev.buf, "n", "<space>u", "<Cmd>GdbFrameUp<CR>",
                        { noremap = true, silent = true })
                    vim.api.nvim_buf_set_keymap(ev.buf, "n", "<space>d", "<Cmd>GdbFrameDown<CR>",
                        { noremap = true, silent = true })
                end
            end
        })
    end
}
use {
    'tanvirtin/monokai.nvim',
    config = function()
        require('monokai').setup { palette = require('monokai').pro }
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
    config = function()
        require("mason").setup()
        -- Mason adds its executables to PATH during setup.
        for _, server in ipairs({ 'clangd', 'lua_ls', 'ts_ls', 'pylsp', 'cmake' }) do
            local config = vim.lsp.config[server]
            if config and type(config.cmd) == "table" and vim.fn.executable(config.cmd[1]) == 1 then
                vim.lsp.enable(server)
            end
        end
    end
}
use {
    'williamboman/mason-lspconfig.nvim',
    config = function()
        require("mason-lspconfig").setup({
            automatic_enable = false
        })

    end
}
use {
    'mfussenegger/nvim-dap',
    config = function()
        local wk = require("which-key")
        wk.add({
            { "<leader>d", group = "debug" },
            { "<leader>da", function() require("dap").continue() end, desc = "DAP Debug" },
            { "<leader>db", function() require("dap").toggle_breakpoint() end, desc = "DAP Toggle Breakpoint" },
            { "<leader>dx", function() require("dap").run_last() end, desc = "DAP Run Last" },
            { "<leader>dc", function() require("dap").run_to_cursor() end, desc = "DAP Run until cursor" },
        })

        local dap = require('dap')
        dap.adapters.gdb = {
            id = 'gdb',
            type = 'executable',
            command = 'gdb',
            args = { '--quiet', '--interpreter=dap' },
        }
        dap.configurations.c = {
            {
                name = 'Run executable (GDB)',
                type = 'gdb',
                request = 'launch',
                program = function()
                    local path = vim.fn.input({
                        prompt = 'Path to executable: ',
                        default = vim.fn.getcwd() .. '/',
                        completion = 'file',
                    })

                    return (path and path ~= '') and path or dap.ABORT
                end,
            },
            {
                name = 'Run executable with arguments (GDB)',
                type = 'gdb',
                request = 'launch',
                program = function()
                    local path = vim.fn.input({
                        prompt = 'Path to executable: ',
                        default = vim.fn.getcwd() .. '/',
                        completion = 'file',
                    })

                    return (path and path ~= '') and path or dap.ABORT
                end,
                args = function()
                    local args_str = vim.fn.input({
                        prompt = 'Arguments: ',
                    })
                    return vim.split(args_str, ' +')
                end,
            },
            {
                name = 'Attach to process (GDB)',
                type = 'gdb',
                request = 'attach',
                processId = require('dap.utils').pick_process,
            },
        }
        dap.configurations.cpp = dap.configurations.c
    end
}
use { 'jay-babu/mason-nvim-dap.nvim',
    config = function()
        require('mason-nvim-dap').setup {
            automatic_installation = false,
            handlers = {
                node2 = function(config)
                    config.adapters = {
                        type = 'executable',
                        command = vim.fn.exepath('node-debug2-adapter'),
                    }
                    config.configurations[#config.configurations + 1] = {
                        type = 'node2',
                        request = 'launch',
                        name = 'Debug Jest Tests',
                        runtimeExecutable = 'node',
                        runtimeArgs = {
                            './node_modules/jest/bin/jest.js',
                            '--runInBand',                       -- Run tests serially (important for debugging)
                            '--no-cache',                        -- Disable cache to ensure latest code runs
                        },
                        args = { '${fileBasenameNoExtension}' }, -- Run tests in the current file
                        cwd = vim.fn.getcwd(),
                        console = 'integratedTerminal',
                        protocol = "inspector",
                        internalConsoleOptions = 'neverOpen',
                        sourceMaps = true,
                    }
                    require('mason-nvim-dap').default_setup(config)
                end
            },
            ensure_installed = { 'js', 'node2' }
        }
    end
}
use 'mfussenegger/nvim-dap-python'
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

use { 'nvim-neotest/nvim-nio' }
use {
    'rcarriga/nvim-dap-ui',
    config = function()
        local dap = require("dap")
        local dapui = require("dapui")
        dapui.setup()
        dap.listeners.after.event_initialized["dapui_config"] = function()
            dapui.open()
        end
        dap.listeners.before.event_terminated["dapui_config"] = function()
            dapui.close()
        end
        dap.listeners.before.event_exited["dapui_config"] = function()
            dapui.close()
        end

        local wk = require("which-key")
        wk.add({
            { "<leader>du", function() require("dapui").toggle() end, desc = "DAP UI Toggle" },
        })

        vim.api.nvim_create_autocmd({ "FileType" }, {
            callback = function(ev)
                if type(ev.match) == "string" and string.match(ev.match, "^dapui") then
                    vim.api.nvim_buf_set_keymap(ev.buf, "n", "<space>n", "<Cmd>lua require'dap'.step_over()<CR>",
                        { noremap = true, silent = true })
                    vim.api.nvim_buf_set_keymap(ev.buf, "n", "<space>s", "<Cmd>lua require'dap'.step_into()<CR>",
                        { noremap = true, silent = true })
                    vim.api.nvim_buf_set_keymap(ev.buf, "n", "<space>o", "<Cmd>lua require'dap'.step_out()<CR>",
                        { noremap = true, silent = true })
                    vim.api.nvim_buf_set_keymap(ev.buf, "n", "<space>c", "<Cmd>lua require'dap'.continue()<CR>",
                        { noremap = true, silent = true })
                    vim.api.nvim_buf_set_keymap(ev.buf, "n", "<space>t",
                        "<Cmd>lua require'dap'.close(); require('dapui').close()<CR>",
                        { noremap = true, silent = true })
                    vim.api.nvim_buf_set_keymap(ev.buf, "n", "<space>p", "<Cmd>lua require'dap'.pause()<CR>",
                        { noremap = true, silent = true })
                    vim.api.nvim_buf_set_keymap(ev.buf, "n", "<space>u", "<Cmd>lua require'dap'.up()<CR>",
                        { noremap = true, silent = true })
                    vim.api.nvim_buf_set_keymap(ev.buf, "n", "<space>d", "<Cmd>lua require'dap'.down()<CR>",
                        { noremap = true, silent = true })
                end
            end
        })
    end
}

if major == 0 and minor <= 10 then
    use {
        'nvim-treesitter/nvim-treesitter',
        config = function()
            require 'nvim-treesitter.configs'.setup {
                ensure_installed = { "c", "cpp", "python", "lua", "vim", "cmake" },
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
end

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
    'ibhagwan/fzf-lua',
    config = function()
        nnoremap("<c-p>", "<cmd>FzfLua files<cr>")
        require('fzf-lua').setup({
            files = {
                find_opts = [[-type f \! -path '*/.git/*' \! -path '*/.cache/*' \! -path './build*']]
            }
        })
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
    "nvim-telescope/telescope-dap.nvim",
    config = function()
        require("telescope").load_extension("dap")
    end
}
use {
    'lidotcircle/nvim-repl',
    config = function()
        nmap("<leader>ax", "<Plug>(nvim-repl-current-line)")
        nmap("<leader>af", "<Plug>(nvim-repl-current-file)")
        vmap("<leader>aa", "<Plug>(nvim-repl-selection)")
        nmap("<leader>ar", "<Plug>(nvim-repl-reset-interpreter)")
        nmap("<leader>ac", "<Plug>(nvim-repl-win-close)")
        nmap("<leader>ao", "<Plug>(nvim-repl-win-open)")
        nmap("<leader>at", "<Plug>(nvim-repl-win-toggle)")
        nmap("<leader>al", "<Plug>(nvim-repl-buffer-clear)")
        nmap("<leader>as", "<Plug>(nvim-repl-buffer-close)")
        nmap("<leader>am", "<Plug>(nvim-repl-toggle-internal-external-mode)")
        nmap("<leader>ap", "<Plug>(nvim-repl-show-prompt)")
        nmap("<leader>ab", "<Plug>(nvim-repl-show-prompt-bash)")
        nmap("<leader>aa", "<Plug>(nvim-repl-show-sessions)")
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
        require("trouble").setup()
        nnoremap("<leader>xx", "<cmd>Trouble diagnostics toggle<cr>")
        nnoremap("<leader>xw", "<cmd>Trouble diagnostics toggle<cr>")
        nnoremap("<leader>xd", "<cmd>Trouble diagnostics toggle filter.buf=0<cr>")
        nnoremap("<leader>xq", "<cmd>Trouble qflist toggle<cr>")
        nnoremap("<leader>xl", "<cmd>Trouble loclist toggle<cr>")
        nnoremap("<leader>gR", "<cmd>Trouble lsp_references toggle<cr>")
    end
}
use 'f-person/git-blame.nvim'
use 'sindrets/diffview.nvim'

-- vim.cmd('let g:copilot_proxy = "http://localhost:11435"')
-- vim.cmd('let g:copilot_proxy_strict_ssl = v:false')
-- use {
--     'github/copilot.vim',
--     config = function()
--         vim.g.copilot_no_tab_map = true
--         vim.api.nvim_set_keymap("i", "<C-J>", 'copilot#Accept("<CR>")', { silent = true, expr = true })
--     end
-- }
-- use {
--     'olimorris/codecompanion.nvim',
--     config = function()
--         require("codecompanion").setup({
--             opts = {
--                 log_level = "DEBUG", -- or "TRACE"
--             }
--         })
--     end
-- }

use {
    'NTBBloodbath/galaxyline.nvim',
    config = function()
        require("hula.plugins.galaxyline").setup()
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
    "akinsho/toggleterm.nvim",
    config = function()
        require("toggleterm").setup()
        local Terminal = require('toggleterm.terminal').Terminal
        local lazygit  = Terminal:new({
            cmd = "lazygit",
            direction = "float",
            hidden = true,
        })

        function G_lazygit_toggle()
            lazygit:toggle()
        end

        vim.api.nvim_set_keymap("n", "<leader>u", "<cmd>lua G_lazygit_toggle()<CR>", {
            noremap = true,
            silent = true
        })
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
if has_ultisnips then
    use 'quangnguyen30192/cmp-nvim-ultisnips'
end
use 'gennaro-tedesco/nvim-peekup'

return mgr
