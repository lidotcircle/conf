local wk = require("which-key")
wk.register({
    d = {
        a = { function() require("dap").continue() end, "DAP Debug"}
    }
}, { prefix = "<leader>" })

