local M = {}

function M.setup()
    require('sidekick').setup({
        nes = { enabled = false },
        cli = {
            mux = { backend = 'tmux', enabled = true },
        },
    })
    vim.opt.autoread = true

    local cli = require('sidekick.cli')
    require('which-key').add({
        { '<leader>A', group = 'AI agent' },
        { '<leader>Aa', function() cli.toggle({ name = 'codex', focus = true }) end, desc = 'Toggle Codex' },
        { '<leader>Af', function() cli.send({ name = 'codex', msg = '{file}' }) end, desc = 'Send file to Codex' },
        { '<leader>Av', function() cli.send({ name = 'codex', msg = '{selection}' }) end, mode = 'x', desc = 'Send selection to Codex' },
        { '<leader>Ap', function()
            cli.prompt({ cb = function(_, text)
                if text then cli.send({ name = 'codex', text = text }) end
            end })
        end, mode = { 'n', 'x' }, desc = 'Choose AI prompt' },
        { '<leader>Ad', function() cli.hide({ name = 'codex' }) end, desc = 'Hide Codex' },
        { '<leader>As', function() cli.select({ filter = { installed = true } }) end, desc = 'Select installed AI tool' },
    })
end

return M
