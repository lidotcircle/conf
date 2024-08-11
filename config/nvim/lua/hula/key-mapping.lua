local wk = require("which-key")


local whichkeyMap = vim.api.nvim_eval("g:which_key_map")
if whichkeyMap then
    for k,v in pairs(whichkeyMap) do
        if #k == 1 then
            keymap = {}
            newkey = {}
            for _k2,v2 in pairs(v) do
                k2 = tostring(_k2)
                if type(k2) == 'string' and #k2 == 1 and
                   type(v2) == 'table' and #v2 == 2 and
                   type(v2[1]) == 'string' and string.sub(v2[1], 0, 1) == ':' then
                    newkey[k2] = { v2[1] .. "<CR>", v2[2] }
                end
                if k2 == 'name' then
                    newkey[k2] = v2
                end
            end
            keymap[k] = newkey
            wk.register(keymap, { prefix = "<leader>"})
        end
    end
end

wk.register({
  f = {
    name = "file", -- optional group name
    f = { "<cmd>Telescope find_files<cr>", "Find File" }, -- create a binding with label
    b = { function() print("bar") end, "Foobar" } -- you can also pass functions!
  },
}, { prefix = "<leader>" })
