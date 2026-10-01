local M = {}

function M.setup()
    if not vim.lsp.get_clients then
        return
    end

    -- Legacy plugins expect clients indexed by ID rather than a list.
    -- Use the current API until those plugins migrate their callers.
    vim.lsp.buf_get_clients = function(bufnr)
        local clients = {}
        for _, client in ipairs(vim.lsp.get_clients({ bufnr = bufnr or 0 })) do
            clients[client.id] = client
        end
        return clients
    end
end

return M
