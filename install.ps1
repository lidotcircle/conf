# FileName: install.ps1
# Description: For installing my vim config in windows

## Exit status
$EXIT_SUCCESS=$true;
$EXIT_FAIL=$false;

## github
$GITHUB="github.com";

### Begin install vim config
$install_flags=0;

function back_inst()
{
    Write-Output "Beign backup and install;";

    ## Initialization the backup directory
    if (Test-Path ./backup){
        Remove-Item -Recurse -Path ./backup;
        New-Item -Path . -Name "backup" -ItemType "directory" > $null;
        New-Item -Path ./backup -Name ".vim" -ItemType "directory" > $null;
    } else {
        New-Item -Path . -Name "backup" -ItemType "directory" > $null;
        New-Item -Path ./backup -Name ".vim" -ItemType "directory" > $null;
    }

## Backup and install the vimrc and $profile
    if (Test-Path $HOME/.vimrc){
        Move-Item -Path $HOME/.vimrc -Destination ./backup;
    }
    Copy-Item -Path ./vimrc -Destination $HOME/.vimrc;
    if (Test-Path $profile){
        Move-Item -Path $profile -Destination ./backup/Profile.ps1;
    }
    if (Test-Path ./Profile.ps1){
        Copy-Item -Path ./Profile.ps1 -Destination $profile;
    }

## Backup and install the .vim directory
    if (-not (Test-Path $HOME/.vim)){
        New-Item -Path $HOME -name ".vim" -ItemType "directory" > $null;
    }
    foreach ($vimBaItem in (Get-ChildItem -Path $HOME/.vim)){
        if ($vimBaItem -NotMatch "^bundle"){
            Move-Item -Path $HOME/.vim/$vimBaItem -Destination ./backup/.vim;
        }
    }
    foreach ($vimInItem in (Get-ChildItem -Path ./vim)){
        Copy-Item -Recurse -Path ./vim/$vimInItem -Destination $HOME/.vim;
    }

## Backup and install the texrc.tex and .tex directory
    if (Test-Path $HOME/texrc.tex){
        Move-Item -Path $HOME/texrc.tex -Destination ./backup;
    }
    Copy-Item -Path ./texrc.tex -Destination $HOME/texrc.tex;
 
    if (Test-Path $HOME/.tex){
        Move-Item -Path $HOME/.tex -Destination ./backup;
    }
    Copy-Item -Path ./tex -Destination $HOME/.tex;


## Finish backup and install
    Write-Output "Finish backup and install.";
    return;
}

function vim_inst_vundle()
{
## Check whether already install "Vundle.vim" and "vim-powerline"
    Write-Output 'Begin install "Vundle.vim" and "vim-powerline";';
    if ((Test-Path $HOME/.vim/bundle/Vundle.vim) -and (Test-Path $HOME/.vim/bundle/vim-powerline)){
        Write-Output "Plugin have installed.";
        return;
    }

## Check the network
    if (-not (ping -n 1 $GITHUB)){
        Write-Output "can't connect github, so give up installing the plugins.";
        exit $EXIT_FAIL;
    }

## Test git whether install or not.
    try{
        git --version > $null;
    }
    catch{
        Write-Output "Error! You need install git, and add the path of git to PATH env.";
        Write-Output "Exit!";
        exit $EXIT_FAIL;
    }

## Install
    if (-not (Test-Path $HOME/.vim/bundle/Vundle.vim)){
        Write-Output "Begin install Vundle.vim:";
        git clone http://github.com/VundleVim/Vundle.vim $HOME/.vim/bundle/Vundle.vim;
        if ($?){
            Write-Output "Install Vundle.vim successfully.";
        } else {
            Write-Output "Install Vundle.vim fail!";
        }
    } else {
        Write-Output "Already install Vundle.vim.";
    }
    if (-not (Test-Path $HOME/.vim/bundle/vim-powerline)){
        Write-Output "Begin install vim-powerline:";
        git clone http://github.com/Lokaltog/vim-powerline $HOME/.vim/bundle/vim-powerline;
        if ($?){
            Write-Output "Install vim-powerline successfully.";
        } else {
            Write-Output "Install vim-powerline fail!";
        }
    } else {
        Write-Output "Already install vim-powerline.";
    }
    return;
}
## Main part
while ($true){
    back_inst;
    vim_inst_vundle;
    Write-Output "Finish!";
    break;
}

## Finish
exit $EXIT_SUCCESS;
