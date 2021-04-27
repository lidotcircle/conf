#!/usr/bin/env bash

# load color functions
source ./bash/functions
source $(dirname ${BASH_SOURCE[0]})/script/utils.sh


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

# Check the dependencies
assert "[ -n "$(which git)" ]" "require git"
assert "[ -n "$(which curl)" ]" "require curl"

GIT_UPDATE='git fetch origin >> /dev/null && git merge origin/master >>/dev/null && git submodule update --init --recursive >> /dev/null'
GIT_REP='https://github.com/'

declare -A INSTALL_FILES=()
declare -A INSTALL_DIRS=()
declare -A GITHUB_REPO=()

INSTALL_FILES["$PWD/vimrc"]="${HOME}/.vimrc"
INSTALL_FILES["$PWD/config/nvim/init.vim"]="${HOME}/.config/nvim/init.vim"
INSTALL_FILES["$PWD/Misc/ycm_extra_conf.py"]="${HOME}/.ycm_extra_conf.py"

INSTALL_FILES["$PWD/vscode/keybindings.json"]="${HOME}/.config/Code/User/keybindings.json"
INSTALL_FILES["$PWD/vscode/settings.json"]="${HOME}/.config/Code/User/settings.json"

INSTALL_FILES["$PWD/bashrc_main"]="${HOME}/.bashrc"
INSTALL_FILES["$PWD/bashrc_main"]="${HOME}/.bashrc_main"
INSTALL_FILES["$PWD/inputrc"]="${HOME}/.inputrc"

INSTALL_DIRS["$PWD/vim/colors"]="${HOME}/.vim/colors"
INSTALL_DIRS["$PWD/vim/vim-conf"]="${HOME}/.vim/vim-conf"
INSTALL_DIRS["$PWD/vim/UltiSnips"]="${HOME}/.vim/UltiSnips"
INSTALL_DIRS["$PWD/vim/self-plugins"]="${HOME}/.vim/self-plugins"

GITHUB_REPO["tmux-plugins/tpm"]=${HOME}/.tmux/plugins
GITHUB_REPO["tmux-plugins/tmux-sensible"]=${HOME}/.tmux/plugins

# install_file() $src_file [$dst_file | $dst_dir] #{
install_file()
{
    assert "[ $# -eq 2 ]"
    assert "[ -f $1 ]" "file $1 doesn't exist"

    [ ! -e $2 ] || rm -f $2
    assert "[ $? -eq 0 ]" "clean file '$2' fail"

    if [ -d $2 ]; then
        cp -rf $1 $2
    else
        [ -d ${2%/*} ] || mkdir -p ${2%/*} && ln -s $1 $2
    fi
    assert "[ $? -eq 0 ]" "install file '$1' fail"
}
#}
# install_directory() #{
install_directory()
{
    assert "[ $# -eq 2 ]"
    assert "[ -d $1 ]" "directory $1 doesn't exist"

    [ ! -d $2 ] || rm -rf $2
    assert "[ $? -eq 0 ]" "clean $2 fail"

    [ ! -d ${2%/*} ] || mkdir -p ${2%/*} && ln -s $1 $2
    assert "[ $? -eq 0 ]" "install directory '$1' fail"
}
#}
#{ function : install_github_repo()
install_github_repo()
{
    assert "[ $# -eq 2 ]"
    pushd $2 >> /dev/null
    assert "[ $? -eq 0 ]" "'pushd $2' fail"

    if [ -e ${1##*/} ]; then
        info "'${1##*/}' have installed, just update it, proccess..."
        pushd ${1##*/} >> /dev/null
        assert "[ $? -eq 0 ]" "'pushd ${1##*/}' fail"

        eval $GIT_UPDATE >> /dev/null
        if [ $? -eq 0 ]; then
            info "update ${1##*/} success." && popd >> /dev/null
        else
            warn "update ${1##*/} fail, remove it and continue to install it." && popd && rm -rf ${1##*/}
        fi
    else
        info "cloning ${GIT_REP}${1}"
        git clone --recurse-submodules ${GIT_REP}${1} >> /dev/null
        assert "[ $? -eq 0 ]" "clone repo '$1' fail"
        info "$1 finish"
    fi

    popd >> /dev/null
}
#}

# InstallVimPlug #{
function InstallVimPlug 
{
    info "install/update vim-plug"
    curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
        https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim >> /dev/null 2>&1
    assert "[ $? -eq 0 ]" "fail to install/update vim-plug"
} #}
#{ IntallBashIt()
InstallBashIt()
{
    if [ ! -f ./otherRepo/bash-it/install.sh ]; then
        warn "please init submodules, 'git submodule update --init'"
        return 1
    fi
    info "setup bash-it"
    rm -f $HOME/.bashrc.bak
    ./otherRepo/bash-it/install.sh --silent >> /dev/null && \
    echo "source $HOME/.bashrc_main" >> $HOME/.bashrc
    assert "[ $? -eq 0 ]" "bash-it fatal error"
    return 0
}
#}
# InstallFiles() #{
function InstallFiles() 
{
    info "install files"
    for file in "${!INSTALL_FILES[@]}"; do
        local dst=${INSTALL_FILES[$file]}
        debug "file: [ $file -> $dst ]"
        install_file $file $dst
    done
} #}
# InstallDirectories() #{
function InstallDirecotories() 
{
    info "install directories"
    for src in "${!INSTALL_DIRS[@]}"; do
        local dst=${INSTALL_DIRS[$src]}
        debug "directory: [ $src -> $dst ]"
        install_directory $src $dst
    done
} #}
# InstallGitRepo() #{
function InstallGitRepo() 
{
    info "install git repos"
    for repo in "${!GITHUB_REPO[@]}"; do
        local dst=${GITHUB_REPO[$repo]}
        install_github_repo  $repo $dst
    done
} #}

while (true); do
    InstallVimPlug
    InstallFiles
    InstallDirecotories
    InstallGitRepo
    InstallBashIt
    break;
done

