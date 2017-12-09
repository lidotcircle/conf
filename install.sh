#!/usr/bin/env bash

#{ Check the dependencies
## git
[ -z $(which git) ] && echo "Need install git, exit." && return 1 || clear
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
#{ Install list
Plugin_list=("VundleVim/Vundle.vim" \
    "Lokaltog/vim-powerline")

File_list=("./vimrc|${HOME}/.vimrc" \
    "./bashrc|${HOME}/.bashrc" \
    "./texrc.tex|${HOME}/texrc.tex" \
    "./vim/colors|${HOME}/.vim/colors" \
    "./vim/pri-vim|${HOME}/.vim/pri-vim" \
    "./vim/pri-plugins|${HOME}/.vim/pri-plugins" \
    "./bash|${HOME}/.bash" \
    "./tex|${HOME}/.tex")
#}
## WARNING and ERROR functions
#{ function : __warning()
__warning()
{
    __warning "< ${1} >" && warning_count=$[$warning_count + 1] && return 0
}
#}
#{ function : __error()
__error()
{
    __error "< ${1} >" && error_count=$[$error_count + 1] && return 0
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
    [ -e $2 ] && mv -f $2 $BACKUP_DIRE >> /dev/null || \
        __warning "backup file $2 fail, but continue"

    # final coppy $1 to $2
    cp -rf $1 $2 && return $SUCCESS >> /dev/null || \
        (__error "install \"$1\" to \"$2\" FAILED." && return $FAIL)
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
#{ Main proccess
while (true); do
    echo "**REMOVE** previous backup." && rm -rf ./backup && mkdir ./backup
    echo "***BEGIN** install files..."
    for __file in ${File_list[@]}; do
        install_fil ${__file%%\|*} ${__file##*\|}
    done
    echo "**FINISH** install files!"

    echo "***BEGIN** install vim plugins..."
    for __plug in ${Plugin_list[@]}; do
        install_plug ${__plug}
    done
    echo "**FINISH** install vim plugins!"

    echo "**FINISH** Install, total ERROR is $error_count, total WARNING is $error_count"
    [ $error_count -eq 0 ] && exit $SUCCESS || exit $FAIL
done
#}
