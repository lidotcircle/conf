local wk = require("which-key")
local mappings = {}

-- Translate the shared Vim mappings into which-key's current format.
local function add_mappings(prefix, entries)
    for key, value in pairs(entries) do
        if key == "name" then
            table.insert(mappings, { prefix, group = value })
        elseif type(value) == "table" then
            local lhs = prefix .. tostring(key)
            if type(value[1]) == "string" and type(value[2]) == "string" then
                local rhs = value[1]
                if rhs:sub(1, 1) == ":" then
                    rhs = rhs .. "<CR>"
                end
                table.insert(mappings, { lhs, rhs, desc = value[2] })
            else
                add_mappings(lhs, value)
            end
        end
    end
end

add_mappings("<leader>", vim.g.which_key_map or {})
wk.add(mappings)
wk.add({ { "<leader>f", group = "file" } })
