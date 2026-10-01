local pluginList = {}
local M = {}

local function addPluginWithPlug(vimFunc, pluginName)
    vim.api.nvim_exec2("call " .. vimFunc .. "('" .. pluginName .. "')", { output = false })
end

local callbackList = {}
local function addPlugin(vimFunc, pluginInfo)
    if type(pluginInfo) == 'string' then
        addPluginWithPlug(vimFunc, pluginInfo)
    elseif type(pluginInfo) == 'table' then
        if pluginInfo.requires then
            for _, i in ipairs(pluginInfo.requires) do
                addPlugin(vimFunc, i)
            end
        end
        addPluginWithPlug(vimFunc, pluginInfo[1])
        if pluginInfo.config then
            table.insert(callbackList, pluginInfo.config)
        end
    end
end

local function log(...)
end

function M.AddPluginsWithVimFunction(vimFunctionName)
    for _, pluginInfo in ipairs(pluginList) do
        log("add plugin " .. vim.inspect(pluginInfo))
        addPlugin(vimFunctionName, pluginInfo)
    end
end

function M.AfterPluginsLoaded()
    for _, callback in ipairs(callbackList) do
        callback()
    end
end

function M.use(plugin)
    table.insert(pluginList, plugin)
end

return M
