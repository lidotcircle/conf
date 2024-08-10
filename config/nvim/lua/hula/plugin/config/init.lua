_G["plugcallbacks"] = {}
local plugcallbacks = _G["plugcallbacks"]
local lbss = _G["loaded_but_not_setup_plugs"] or {}

local plugins = {
    "neodev-nvim";
    "nvim-lspconfig";
    "nvim-cmp";
    "mason-lspconfig.nvim";
    "copilot.vim";
    "neovim-session-manager";
    "nvim-treesitter";
    "which-key-nvim";
    "Comment-nvim";
    "symbols-outline-nvim";
    "nvim-dap";
    "nvim-dap-go";
    "neogit";
}

for _,i in ipairs(plugins) do
    local sani = i:gsub("%.", "-")
    local cb = require("hula.plugin.config." .. sani)
    plugcallbacks[i] = cb;

    if lbss[i] and type(cb) == 'function' then
        cb()
    end
end
