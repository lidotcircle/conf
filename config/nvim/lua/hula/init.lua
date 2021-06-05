local lspconfig = require('lspconfig')


local on_attach = function(client, bufnr)
    require('completion').on_attach(client)
    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end

    -- Mappings.
    local opts = { noremap=true, silent=true }
    buf_set_keymap('i', '<c-space>',  '<Cmd>lua vim.lsp.buf.completion()<CR>', opts)
    buf_set_keymap('n', '<leader>gD', '<Cmd>execute "normal m\'" | lua vim.lsp.buf.declaration()<CR>', opts)
    buf_set_keymap('n', '<leader>go', '<Cmd>execute "normal m\'" | lua vim.lsp.buf.definition()<CR>', opts)
    buf_set_keymap('n', '<leader>K',  '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    buf_set_keymap('n', '<leader>gi', '<cmd>execute "normal m\'" | lua vim.lsp.buf.implementation()<CR>', opts)
    buf_set_keymap('n', '<leader>gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
    buf_set_keymap('n', '<leader>gh', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    buf_set_keymap('n', '<leader>la', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
    buf_set_keymap('n', '<leader>lr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
    buf_set_keymap('n', '<leader>ll', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)
    buf_set_keymap('n', '<leader>tD', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
    buf_set_keymap('n', '<leader>rn', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)
    buf_set_keymap('n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    buf_set_keymap('n', '<leader>ce', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
    buf_set_keymap('n', '<leader>[',  '<cmd>execute "normal m\'" | lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
    buf_set_keymap('n', '<leader>]',  '<cmd>execute "normal m\'" | lua vim.lsp.diagnostic.goto_next()<CR>', opts)
    buf_set_keymap('n', '<space>q',   '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
end

lspconfig.sumneko_lua.setup {
    cmd = {
        os.getenv("HOME") .. "/lua-language-server/bin/Linux/lua-language-server",
        "-E",
        os.getenv("HOME") .. "/lua-language-server/main.lua"
    };
    settings = {
        Lua = {
            runtime = {
                -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
                version = 'LuaJIT',
                -- Setup your lua path
                path = vim.split(package.path, ';'),
            },
            diagnostics = {
                -- Get the language server to recognize the `vim` global
                globals = {'vim'},
            },
            workspace = {
                -- Make the server aware of Neovim runtime files
                library = {
                    [vim.fn.expand('$VIMRUNTIME/lua')] = true,
                    [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
                },
            },
            -- Do not send telemetry data containing a randomized but unique identifier
            telemetry = {
                enable = false,
            },
        },
    };
    on_attach = on_attach;
}

lspconfig.bashls.setup{
    on_attach = on_attach;
}
lspconfig.vimls.setup{
    on_attach = on_attach;
}
lspconfig.pyright.setup{
    on_attach = on_attach;
}

