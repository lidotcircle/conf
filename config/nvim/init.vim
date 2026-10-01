
set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath=&runtimepath
lua require('hula.lsp_compat').setup()
source ~/.vimrc

lua require('hula.init')
