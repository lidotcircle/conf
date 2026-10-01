local M = {}

function M.setup()
    local condition = require('galaxyline.condition')
    local lsp = require('galaxyline.providers.lsp')
    local diagnostic = require('galaxyline.providers.diagnostic')

    condition.check_active_lsp = function()
        return #vim.lsp.get_clients({ bufnr = 0 }) > 0
    end

    lsp.get_lsp_client = function(message, ignored_servers)
        local names = {}
        for _, client in ipairs(vim.lsp.get_clients({ bufnr = 0 })) do
            if not vim.tbl_contains(ignored_servers or {}, client.name) then
                table.insert(names, client.name)
            end
        end
        return #names > 0 and table.concat(names, ', ') or (message or 'No Active Lsp')
    end

    diagnostic.get_diagnostic = function(severity)
        if vim.fn.exists('*coc#rpc#start_server') == 1 then
            local kinds = { 'error', 'warning', 'information', 'hint' }
            local ok, info = pcall(vim.api.nvim_buf_get_var, 0, 'coc_diagnostic_info')
            return ok and info[kinds[severity]] or 0
        end
        return #vim.diagnostic.get(0, { severity = severity })
    end

    require('galaxyline.themes.eviline')
    require('galaxyline').load_galaxyline()
end

return M
