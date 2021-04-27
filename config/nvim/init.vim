
set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath=&runtimepath
source ~/.vimrc

lua require'lspconfig'.bashls.setup{}
lua require'lspconfig'.vimls.setup{}

