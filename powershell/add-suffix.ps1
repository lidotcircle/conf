Param(
    [Parameter(Mandatory=$true, Position=0)]
    [ValidateNotNull()]
    $Path,
    [Parameter(Mandatory=$true, Position=1)]
    [ValidateNotNull()]
    [String]
    $Suffix
)

function xadd_suffix()
{
    Param(
        [Parameter(Mandatory=$true, Position=0)]
        [ValidateNotNull()]
        $Path,
        [Parameter(Mandatory=$true, Position=1)]
        [ValidateNotNull()]
        [String]
        $Suffix
    )
    if ($PS_DEBUG_){
            write-output $("Path: " + $Path + "`n" + 
                           "Suffix: " + $Suffix)
    }
    $allItem = $(get-ChildItem -Path $Path)
    foreach ($childItem in $allItem){
        if($childItem.Attributes.toString() -eq "Archive"){
            $originPath = $childItem.FullName.toString()
            $destPath = $childItem.Directory.toString() + "/" + $childItem.BaseName + $Suffix + $childItem.Extension.toString()
            try{
                Move-Item -Path $originPath -Destination $destPath
                if ($PS_DEBUG_){
                    write-output $("||| MOVE <" + $originPath + "> to <" + $destPath + ">")
                }
            }
            catch [IOExceptioni] {
                if ($PS_DEBUG_){
                    write-output $("!!! FAIL IN <" + $originPath + ">\n" + $_.Exception.Message)
                }
            }
        }
    }
}

xadd_suffix -Path $Path -Suffix $Suffix