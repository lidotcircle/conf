# FileName: install.ps1
# Description: For installing my vim config in windows

#{ Some variables
## init localtion
$init_localtion=$(get-location).Path;
## Exit status
$EXIT_SUCCESS = $true;
$EXIT_FAIL = $false;

## git repository prefix
$GITREP = "https://github.com/";

## backup directory
$BACKUP = "./backup"

## Install file dictionary
$Install_dict = @{"./vimrc"="$HOME/.vimrc";
"./Profile.ps1"="$profile";
"./texrc.tex"="$HOME/texrc.tex";
"./Misc/acadrc.lsp"="$HOME/acadrc.lsp";
"./Misc/vsvimrc"="$HOME/.vsvimrc";
"./Misc/ycm_extra_conf.py"="$HOME/.ycm_extra_conf.py";
"./vim/colors"="$HOME/.vim/colors";
"./vim/self-plugins"="$HOME/.vim/self-plugins";
"./vim/vim-conf"="$HOME/.vim/vim-conf";
"./tex"="$HOME/.tex";
"./asyLib"="$HOME/asyLib";
"./vscode/snippets"="$env:APPDATA/Code/User/snippets";
"./vscode/keybindings.json"="$env:APPDATA/Code/User/keybindings.json";
"./vscode/settings.json"="$env:APPDATA/Code/User/settings.json"
};

## vim plugin list
$Plugin_list = @("VundleVim/Vundle.vim", "Lokaltog/vim-powerline");
## vim plugin local location
$PLU_INS = "$HOME/.vim/bundle";
#}

#{ Dependencies check.
while($true){
    try{
        # supressed output.
        git --version >> $null
    }
    catch {
        Write-Output "";
        exit $EXIT_FAIL;
    }
    clear; break;
}
#}

# usage : install_fil <src> <dest>
#{ function : install_fil()
function install_fil()
{
    if(-not $args.count -eq 2){
        Write-Error -Message "Argument Error in install_fil()." -Category "InvalidArgument";
    }
    if(Test-path $args[1].toString()){
        move-item $args[1].toString() $BACKUP;
    }
    if(Test-Path -Path $args[0] -PathType leaf){
        New-Item -ItemType HardLink -Path $args[1] -Value $args[0] >> $null;
    } elseif (Test-Path -Path $args[0] -PathType Container){
        New-Item -ItemType Junction -Path $args[1] -Value $args[0] >> $null;
    }
#    Copy-Item -Force -Recurse $args[0].toString() $args[1].toString();
}
#} End function

# usage : install_plug <git_rep>
#{ function : install_plug()
function install_plug()
{
    if(-not $args.count -eq 1){
        Write-Error -Message "Argument Error in install_plug()." -Category "InvalidArgument";
    }
    $Plug = $args[0].toString(); $plug_name = $Plug.remove(0, $Plug.indexOf('/') + 1);
    push-location $PLU_INS;
    if(Test-Path $plug_name){
        push-location $plug_name;
        if(test-path ".git"){
            Write-Output ("Plugin <{0}> already exist, skip it." -f $plug_name);
            return $true >> $null;
        } else {
            pop-location; remove-item -Force -recurse $plug_name;
        }
        Write-Output "Install plugin <$plug_name>.";
        $Plug_address = $GITREP + $Plug;
        echo $Plug_address;
        git clone --recurse-submodules $Plug_address >> $null;
        if(Test-path $plug_name){
            return $true >> $null;
        } else {
            Write-Warning "Install <$plug_name> failed.";
            return $false >> $null;
        }
    }
}
#}

# Main process
#{
while($true){
    Write-Output "**REMOVE** backup.";
    if(Test-Path $BACKUP){
        remove-item -Force -Recurse $BACKUP;
        new-item -itemType 'directory' $BACKUP >> $null;
    }
    Write-Output "***BEGIN** install files...";
    foreach($dictEnt in $Install_dict.getEnumerator()){
        install_fil $dictEnt.key $dictEnt.value;
        Write-Output ("File <{0}> to <{1}>." -f $dictEnt.key, $dictEnt.value);
    }
    Write-Output "**FINISH** install files!";
    if(-not (Test-Path $PLU_INS)){
        new-item -itemType 'directory' $PLU_INS >> $null;
    }
    Write-Output "***BEGIN** install plugins...";
    foreach($plug in $Plugin_list.getEnumerator()){
        install_plug $plug;
    }
    Write-Output "**FINISH** install plugins!";
    Write-Output "**FINISH**"; push-location $init_localtion; exit $true;
}
#}
