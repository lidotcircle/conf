#!/usr/bin/env bash

## exit code
SUCCESS=0
FAIL=1

## github
github="github.com"

### Begin install vim config
install_flags=0

back_inst()
{
    echo "Begin backup and install"

	if [ -d ./backup ]
	then
		rm -rf ./backup
		mkdir ./backup
        mkdir ./backup/.vim
	else
		mkdir ./backup
        mkdir ./backup/.vim
	fi

    if [ -d ~/bin ]
    then
        mv ~/bin ./backup/bin
    fi
    if [ ! -d ~/bin ]; then
        mkdir ~/bin
    fi
    cp -r $(find ./bin -maxdepth 1 -type f) ~/bin
    CPU_ARCH=`uname -m`
    if [ ${CPU_ARCH} == "aarch64" ]; then
        cp -r ./bin/aarch64/* ~/bin
    elif [ ${CPU_ARCH} == "x86_64" ]; then
        cp -r ./bin/x86_64/* ~/bin
    fi

	if [ -f ~/.vimrc ]
	then
		mv ~/.vimrc ./backup
	fi
	cp ./vimrc ~/.vimrc

    if [ -f ~/.bashrc ]
    then
        mv ~/.bashrc ./backup
    fi
    cp ./bashrc ~/.bashrc

    if [ -d ~/.bash ]
    then
        mv ~/.bash ./backup
    fi
    cp -r ./bash ~/.bash

	if [ -d ~/.vim ]
	then
        for fff in ~/.vim/*
        do
            if [ ! ${fff##*\/} = "bundle" ]
            then
                mv ${fff} ./backup/.vim
            fi
        done

        for aaa in ./vim/*
        do
            cp -r ${aaa} ~/.vim
        done
    else
        cp -r ./vim ~/.vim
	fi

    if [ -f ~/texrc.tex ]
    then
        mv ~/texrc.tex ./backup
    fi
    cp texrc.tex ~/

    if [ -d ~/.tex ]
    then
        mv ~/.tex ./backup
    fi
    cp -r ./tex ~/.tex

    echo "End backup and install"

	install_flags=1
}

vim_inst_vundle()
{
	if [ -d ~/.vim/bundle/Vundle.vim ]&&[ -d ~/.vim/bundle/vim-powerline ]
	then
		echo "Plugins have installed."
		return 0
	fi

	if [ ${install_flags} -eq 0 ]
	then
		echo "run install config function first"
		exit ${FAIL}
	fi
	if !(ping -c 1 ${github} > /dev/null)
	then
		echo "can't connect to github.com"
		exit ${FAIL}
	fi
	if [ $(which git) = "" ]
	then
		echo "need install git first"
		exit ${FAIL}
	fi

	echo "Begin install plugins"
	if [ ! -d ~/.vim/bundle ]
	then
		mkdir ~/.vim/bundle
	fi
## Install Plugins
### Vundle
    if [ ! -d ~/.vim/bundle/Vundle.vim ]
    then
        echo "Begin install Vundle.vim:"
        git clone http://github.com/VundleVim/Vundle.vim ~/.vim/bundle/Vundle.vim       # install vundle
    fi
### vim-powerline
    if [ ! -d ~/.vim/bundle/vim-powerline ]
    then
        echo "Begin install vim-powerline:"
        git clone http://github.com/Lokaltog/vim-powerline ~/.vim/bundle/vim-powerline  # install vim-powerline
    fi

    echo "End install plugins"
}

# Main part
while true; do
    back_inst
    vim_inst_vundle
    break
done

exit ${SUCCESS}
### End vim install config
