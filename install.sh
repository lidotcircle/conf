#!/usr/bin/env bash

#{ Check the dependencies
## git
if [[ $0 =~ ^.*install\.sh$ ]]; then
    [ -z $(which git) ] && echo "Need install git, exit." && return 1 || clear
fi
#}
#{ Some variables
## Exit status
readonly SUCCESS=0
readonly FAIL=1

## Backup directory
readonly BACKUP_DIRE=./backup

## Git clone flags
GIT_CLONE_FLAGS='--recurse-submodules'
## Git update
GIT_UPDATE='git fetch origin >> /dev/null && git merge origin/master >>/dev/null && git submodule update --init --recursive >> /dev/null'
## Git clone address
GIT_REP='https://github.com/'

## Plugins directory
PLUG_DEST=${HOME}/.vim/bundle

## error counter and warning count
declare error_count=0
declare warning_count=0
#}
#{ OS TYPE
[[ -n $(uname -a | grep "Android") ]] && android=1 || android=0
machine=$(uname -m)
#}
#{ Install list
declare -a Plugin_list=("VundleVim/Vundle.vim" \
    "Lokaltog/vim-powerline")

declare -A File_list=(["$PWD/vimrc"]="${HOME}/.vimrc" \
    ["$PWD/bashrc_main"]="${HOME}/.bashrc_main" \
    ["$PWD/texrc.tex"]="${HOME}/texrc.tex" \
    ["$PWD/tmux.conf"]="${HOME}/.tmux.conf" \
    ["$PWD/vscode/keybindings.json"]="${HOME}/.config/Code/User/keybindings.json" \
    ["$PWD/vscode/settings.json"]="${HOME}/.config/Code/User/settings.json")
[ $android -eq 1 ] && \
    File_list["$PWD/bin/exec_script/android/sudo"]="${HOME}/bin/sudo"

declare -A Dirs_list_slink=(["$PWD/vim/colors"]="${HOME}/.vim/colors" \
    ["$PWD/vim/vim-conf"]="${HOME}/.vim/vim-conf" \
    ["$PWD/vim/self-plugins"]="${HOME}/.vim/self-plugins" \
    ["$PWD/bash"]="${HOME}/.bash" \
    ["$PWD/tex"]="${HOME}/.tex" \
    ["$PWD/asyLib"]="${HOME}/asyLib" \
    ["$PWD/vscode/snippets"]="${HOME}/.config/Code/User/snippets")

declare -A Dirs_Files_slink_list=(["$PWD/bin"]="$HOME/bin" \
    ["$PWD/bin/exec_script"]="$HOME/bin")
[[ $machine =~ [xX]86[_]64 ]] && Dirs_Files_slink_list["$PWD/bin/x86_64"]="$HOME/bin"
#}

# load color functions
source ./bash/functions

## WARNING and ERROR functions
#{ function : __warning()
__warning()
{
    echo -e "$(red WARNING) : $(blueb ${1} )" && \
        warning_count=$[$warning_count + 1] && return 0
}
#}
#{ function : __error()
__error()
{
    echo -e "$(red ERROR) : $(blueb ${1})" && \
        error_count=$[$error_count + 1] && return 0
}
#}

# Install <src> to <dest>, <dest> must be a full filepath,
# not just a directory of destination
#{ function : install_fil()
install_fil()
{
    # parameter check.
    [ $# -eq 2 ] || (__error "parameter error, in install_fil()" && return $FAIL)
    [ -e $1 ] || (__error "File $1 don't exist, exit!"; return $FAIL)

    # if <dest> already exist, move it to backup directory.
    ([ -e $2 ] || [ -h $2 ]) && mv -f $2 $BACKUP_DIRE >> /dev/null || \
        __warning "backup file $2 fail, but continue"

    # final coppy $1 to $2
    # if $1 is a files, then just make symlink
    if [ -d $1 ]; then cp -rf $1 $2; else ln -s $1 $2; fi && return $SUCCESS >> /dev/null || \
        (__error "install \"$1\" to \"$2\" FAILED." && return $FAIL)
}
#}
#{ function : dir_symbol_link()
dir_symbol_link()
{
    [ ${#@} -eq 2 ] || return 1
    ([ -e $2 ] || [ -h $2 ]) && mv $2 $BACKUP_DIRE
    ln -s $1 $2
}
#}

# Install <Vundle> plugin for vim
#{ function : install_plug()
install_plug()
{
    [ $# -eq 1 ] || (__error "Error parameter, in install_plug()." && return $FAIL)
    # pushd to PLUG_DEST
    pushd $PLUG_DEST >> /dev/null || \
        (__error "pushd to $PLUG_DEST fail, exit." && return $FAIL)
    if [ -e ${1##*/} ]; then
        echo "Already installed <${1##*/}> , just update it, proccess..."
        pushd ${1##*/} >> /dev/null || (__error "pushd ${1##*/} failed" && return $FAIL)
        if (eval $GIT_UPDATE >> /dev/null); then
            echo "update ${1##*/} success." && popd >> /dev/null && return $SUCCESS
        else
            __warning "update ${1##*/} fail, remove it and continue to install it." && \
                popd && rm -rf ${1##*/}
        fi
    fi
    echo "Installing ${1#*/}..."
    git clone $GIT_CLONE_FLAGS ${GIT_REP}${1} >> /dev/null && (popd; return $SUCCESS) || \
        (popd && __error "install ${1##*/} failed!" && return $FAIL)
}
#}

#{ function : make_trans()
make_trans()
{
    [ -f ./otherRepo/translate-shell/build/trans ] && return 0
    ([ ! -z $(which awk) ] || [ ! -z $(which gawk) ]) && return 1
    pushd ./otherRepo/translate-shell && make
    popd
}
#}
#{ function : make_bash_it()
make_bash_it()
{
    [ -f ./otherRepo/bash-it/install.sh ] || return 1
    ./otherRepo/bash-it/install.sh --silent
    return 0
}
#}

# Main process
#{ Main proccess
# safely source this files
[[ $0 =~ ^.*install\.sh$ ]] || return 0
while (true); do
    echo -e "$(green REMOVE) previous backup." && rm -rf ./backup && mkdir ./backup
    [ -d $HOME/.vim ] || mkdir $HOME/.vim
    [ -d $PLUG_DEST ] || mkdir $PLUG_DEST
    echo -e "$(green BEGIN) install files..."
    # try to make trans from source
    make_trans >> /dev/null || __warning "build trans failure"
    for __file in ${!File_list[@]}; do
        install_fil ${__file} ${File_list[$__file]}
    done
    for __dir in ${!Dirs_list_slink[@]}; do
        dir_symbol_link $__dir ${Dirs_list_slink[$__dir]}
    done
    for __dir_link in ${!Dirs_Files_slink_list[@]}; do
       dir_files_link $__dir_link ${Dirs_Files_slink_list[$__dir_link]} 
   done
    echo -e "$(green FINISH) install files!"
    
    echo -e "$(green "Install") $(green "bash-it")"
    make_bash_it >> /dev/null || __warning "Install bash-it failure."
    echo "source \$HOME/.bashrc_main" >> $HOME/.bashrc

    echo -e "$(green BEGIN) install vim plugins..."
    for __plug in ${Plugin_list[@]}; do
        install_plug ${__plug}
    done
    echo -e "$(green FINISH) install vim plugins!"

    echo -e "$(green FINISH Installation)"
    [ $error_count -eq 0 ] && exit $SUCCESS || exit $FAIL
done
#}
