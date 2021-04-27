
call plug#begin('~/.vim/bundle')

let s:plugins= [
            \ 'liuchengxu/vim-which-key',
            \ 'neovim/nvim-lspconfig',
            \ 'ycm-core/YouCompleteMe',
            \
            \ 'Lokaltog/vim-powerline',
            \ 'scrooloose/nerdtree',
            \ 'fholgado/minibufexpl.vim',
            \ 'othree/csscomplete.vim',
            \
            \ 'mattn/emmet-vim',
            \ 'SirVer/ultisnips',
            \ 'honza/vim-snippets',
            \ 'mileszs/ack.vim',
            \ 'junegunn/vim-easy-align',
            \
            \ 'PProvost/vim-ps1',
            \ 'leafgarland/typescript-vim',
            \ 'pboettch/vim-cmake-syntax',
            \ 'cakebaker/scss-syntax.vim',
            \ 'digitaltoad/vim-pug',
            \ ]

for plugin in s:plugins
    Plug plugin
endfor

call plug#end()


let plugins_list = [
            \ "$LOADPATH/self-plugins/win-buffer-only"
            \ ]

for plugpath in map(plugins_list, "expand(v:val)")
    exec "set rtp+=".plugpath
endfor

