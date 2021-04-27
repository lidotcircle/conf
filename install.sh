#!/usr/bin/env bash

# load color functions
source ./bash/functions

# TODO
### HANDING OPTIONS #{
#__options=$(getopt "-o avtbsn --long all")
print_usage()
{
    cat <<EOF
Usage: ./install.sh [-avtbsh]

    -a | --all          install all utility tools, which can use with
                        the options --no-network, --no-vimp, --no-tmuxp
         --no-network   ignore something need connect to network
         --no-symlink   install files or dirs instead of symbolic links
    -v | --vim          install everyting about vim
         --no-vimp      ignore vim plugins
    -t | --tmux         install everything about tmux
         --no-tmuxp     ignore tmux plugins
         --tex          some files about TeX
         --vscode       vscode configuration files
         --asymptote    asymptote vector draw library
    -b | --bash-it      install bash-it
    -s | --trans        install translate
    -h | --help         display this information
EOF
}

declare -i SET_ALL=0
declare -i SET_VIM=0
declare -i SET_NVIMP=0
declare -i SET_TMUX=0
declare -i SET_NTMUXP=0
declare -i SET_BASH_IT=0
declare -i SET_TRANS=0
declare -i SET_NO_NET=0
declare -i SET_TEX=0
declare -i SET_VSCODE=0
declare -i SET_ASYMPTOTE=0
declare -i SET_NSYMLINK=0

[ -z "$1" ] && SET_ALL=1
while [ -n "$1" ]; do
    case "$1" in
        -a | --all)
            [ $SET_ALL -eq 0 ] && SET_ALL=1 || echo -e $(red "duplicated option - <$1>");;
        -v | --vim)
            [ $SET_VIM -eq 0 ] && SET_VIM=1 || echo -e $(red "duplicated option - <$1>");;
        --no-vimp)
            [ $SET_NVIMP -eq 0 ] && SET_NVIMP=1 || echo -e $(red "duplicated option - <$1>");;
        -t | --tmux)
            [ $SET_TMUX -eq 0 ] && SET_TMUX=1 || echo -e $(red "duplicated option - <$1>");;
        --no-tmuxp)
            [ $SET_NTMUXP -eq 0 ] && SET_NTMUXP=1 || echo -e $(red "duplicated option - <$1>");;
        -b | --bash-it)
            [ $SET_BASH_IT -eq 0 ] && SET_BASH_IT=1 || echo -e $(red "duplicated option - <$1>");;
        -s | --trans)
            [ $SET_TRANS -eq 0 ] && SET_TRANS=1 || echo -e $(red "duplicated option - <$1>");;
        --tex)
            [ $SET_TEX -eq 0 ] && SET_TEX=1 || echo -e $(red "duplicated option - <$1>");;
        --vscode)
            [ $SET_VSCODE -eq 0 ] && SET_VSCODE=1 || echo -e $(red "duplicated option - <$1>");;
        --asymptote)
            [ $SET_ASYMPTOTE -eq 0 ] && SET_ASYMPTOTE=1 || echo -e $(red "duplicated option - <$1>");;
        --no-network)
            [ $SET_NO_NET -eq 0] && SET_NO_NET=1 || echo -e $(red "duplicated option - <$1>");;
        -h | --help)
            print_usage; exit 0;;
        *) echo -e $(red "Unknow option - <$1>"); print_usage; exit 1;;
    esac
    shift 1
done

# set value to bool
set_value_to_bool()
{
    [ $# -eq 2 ] || echo -e $(red "impossible error...")
    declare -n tmp_ref=$1
    [ $tmp_ref -eq $2 ] &&\
        echo -e $(red "-a | --all is already specified, other options isn't needed.")
    tmp_ref=$2
}

if [ ${SET_ALL} -eq 1 ]; then
    set_value_to_bool SET_VIM 1
    set_value_to_bool SET_TMUX 1
    set_value_to_bool SET_TRANS 1
    set_value_to_bool SET_BASH_IT 1
    set_value_to_bool SET_TEX 1
    set_value_to_bool SET_VSCODE 1
    set_value_to_bool SET_ASYMPTOTE 1
fi
#}

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
VIM_PLUG_DEST=${HOME}/.vim/bundle
TMUX_PLUG_DEST=${HOME}/.tmux/plugins

## error counter and warning count
declare -i error_count=0
declare -i warning_count=0
#}
#{ OS TYPE
[[ -n $(uname -a | grep "Android") ]] && android=1 || android=0
machine=$(uname -m)
#}

#{ function: add_HT_to(src, dst) -- add one hash table to other
add_HT_to()
{
    declare -p $1 > /dev/null 2>&1 || (echo -e $(red "undeclare variable <$1>") && return 1)
    declare -p $2 > /dev/null 2>&1 || (echo -e $(red "undeclare variable <$2>") && return 1)
    local -n tref1=$1
    local -n tref2=$2
    for __key in ${!tref1[@]}; do
        eval "tref2[${__key}]=\${tref1[${__key}]}"
    done
    return 0
}
#}
#{ Install list
declare -a VIM_PLUGIN_LIST=("VundleVim/Vundle.vim" \
    "Lokaltog/vim-powerline")
declare -a TMUX_PLUGIN_LIST=("tmux-plugins/tpm" \
    "tmux-plugins/tmux-sensible")

declare -A VIM_FILE_LIST=(
    ["$PWD/vimrc"]="${HOME}/.vimrc"
    ["$PWD/init.vim"]="${HOME}/init.vim"
)
declare -A TMUX_FILE_LIST=(["$PWD/tmux.conf"]="${HOME}/.tmux.conf")
declare -A TEX_FILE_LIST=(["$PWD/texrc.tex"]="${HOME}/texrc.tex")
declare -A VSCODE_FILE_LIST=(\
    ["$PWD/vscode/keybindings.json"]="${HOME}/.config/Code/User/keybindings.json" \
    ["$PWD/vscode/settings.json"]="${HOME}/.config/Code/User/settings.json")
declare -A FILE_LIST=(["$PWD/bashrc_main"]="${HOME}/.bashrc_main" \
    ["$PWD/inputrc"]="${HOME}/.inputrc")
[ $android -eq 1 ] && \
    FILE_LIST["$PWD/bin/exec_script/android/sudo"]="${HOME}/bin/sudo"

declare -A VIM_DIRS_LIST_SLINK=(["$PWD/vim/colors"]="${HOME}/.vim/colors" \
    ["$PWD/vim/vim-conf"]="${HOME}/.vim/vim-conf" \
    ["$PWD/vim/UltiSnips"]="${HOME}/.vim/UltiSnips" \
    ["$PWD/vim/self-plugins"]="${HOME}/.vim/self-plugins")
declare -A TMUX_DIRS_LIST_SLINK=()
declare -A TEX_DIRS_LIST_SLINK=(["$PWD/tex"]="${HOME}/.tex")
declare -A VSCODE_DIRS_LIST_SLINK=(\
    ["$PWD/vscode/snippets"]="${HOME}/.config/Code/User/snippets")
declare -A ASYMPTOTE_DIRS_LIST_SLINK=(["$PWD/asyLib"]="${HOME}/.asy/asyLib")
declare -A DIRS_LIST_SLINK=(["$PWD/bash"]="${HOME}/.bash")

# base on options to set <FILE_LIST> and <DIRS_LIST_SLINK>
[ ${SET_VIM} -eq 1 ] && add_HT_to VIM_FILE_LIST FILE_LIST &&\
    add_HT_to VIM_DIRS_LIST_SLINK DIRS_LIST_SLINK
[ ${SET_TMUX} -eq 1 ] && add_HT_to TMUX_FILE_LIST FILE_LIST &&\
    add_HT_to TMUX_DIRS_LIST_SLINK DIRS_LIST_SLINK
[ ${SET_TEX} -eq 1 ] && add_HT_to TEX_FILE_LIST FILE_LIST &&\
    add_HT_to TEX_DIRS_LIST_SLINK DIRS_LIST_SLINK
[ ${SET_VSCODE} -eq 1 ] && add_HT_to VSCODE_FILE_LIST FILE_LIST &&\
    add_HT_to VSCODE_DIRS_LIST_SLINK DIRS_LIST_SLINK
[ ${SET_ASYMPTOTE} -eq 1 ] && add_HT_to ASYMPTOTE_DIRS_LIST_SLINK DIRS_LIST_SLINK

declare -A DIRS_FILES_SLINK_LIST=(["$PWD/bin"]="$HOME/bin" \
    ["$PWD/bin/exec_script"]="$HOME/bin")
[[ $machine =~ [xX]86[_]64 ]] && DIRS_FILES_SLINK_LIST["$PWD/bin/x86_64"]="$HOME/bin"
#}

## WARNING and ERROR functions
#{ function : __warning()
__warning()
{
    echo -e "$(red WARNING) : $(blueb ${1} )" && \
        let warning_count++ && return 0
}
#}
#{ function : __error()
__error()
{
    echo -e "$(red ERROR) : $(blueb ${1})" && \
        let error_count++ && return 0
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
    if [ -d $1 ]; then
        cp -rf $1 $2
    else
        [ -d ${2%/*} ] || mkdir -p ${2%/*} && ln -s $1 $2
    fi && return $SUCCESS >> /dev/null || \
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

# Installing vim plugins or tmux plugins from github
#{ function : install_plug()
install_plug()
{
    [ ${#@} -eq 2 ] || (__error "Error parameter, in install_plug()." && return $FAIL)
    # pushd to <parameter 2>
    pushd $2 >> /dev/null || \
        (__error "pushd to $2 fail, exit." && return $FAIL)
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
    [ -f ./otherRepo/translate-shell/README.md ] || return 1
    [ -f ./otherRepo/translate-shell/build/trans ] && return 0
    ([ -z "$(which awk)" ] && [ -z "$(which gawk)" ]) && return 1
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

    # ensure some essential directory is existing.
    [ -d $HOME/.vim ] || mkdir $HOME/.vim
    [ -d $VIM_PLUG_DEST ] || mkdir -p $VIM_PLUG_DEST
    [ -d $TMUX_PLUG_DEST ] || mkdir -p $TMUX_PLUG_DEST

    echo -e "$(green BEGIN) install files..."

    if [ $SET_TRANS -eq 1 ]; then
        # try to make trans from source
        make_trans >> /dev/null || __warning "build trans failure"
    fi

    # install files, dirs, and symlinks
    for __file in ${!FILE_LIST[@]}; do
        install_fil ${__file} ${FILE_LIST[$__file]}
    done
    for __dir in ${!DIRS_LIST_SLINK[@]}; do
        dir_symbol_link $__dir ${DIRS_LIST_SLINK[$__dir]}
    done
    for __dir_link in ${!DIRS_FILES_SLINK_LIST[@]}; do
       dir_files_link $__dir_link ${DIRS_FILES_SLINK_LIST[$__dir_link]} 
   done
    echo -e "$(green FINISH) install files!"
    
    if [ $SET_BASH_IT -eq 1 ]; then
        echo -e "$(green "Install") $(green "bash-it")"
        make_bash_it >> /dev/null || __warning "Install bash-it failure."
        echo "source \$HOME/.bashrc_main" >> $HOME/.bashrc
    fi

    if [ $SET_VIM -eq 1 ] && [ $SET_NVIMP -eq 0 ] ;then
        echo -e "$(green BEGIN) install vim plugins..."
        for __plug in ${VIM_PLUGIN_LIST[@]}; do
            install_plug ${__plug} ${VIM_PLUG_DEST}
        done
        echo -e "$(green FINISH) install vim plugins!"
    fi

    if [ $SET_TMUX -eq 1 ] && [ $SET_NTMUXP -eq 0 ] ;then
        echo -e "$(green BEGIN) install tmux plugins..."
        for __plug in ${TMUX_PLUGIN_LIST[@]}; do
            install_plug ${__plug} ${TMUX_PLUG_DEST}
        done
        echo -e "$(green FINISH) install tmux plugins!"
    fi

    echo -e "$(green FINISH Installation)"
    [ $error_count -eq 0 ] && exit $SUCCESS || exit $FAIL
done
#}
