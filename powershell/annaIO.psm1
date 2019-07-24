<#
.synopsis
"Basic function to manipulate the name of file in specified directory."
#>
function Add-EXtraName()
{
    Param(
        [Parameter(Mandatory = $true, Position = 0)] [ValidateNotNullOrEmpty()]
        $Path, 
        [Parameter(Mandatory = $true, Position = 1)] [String]
        $Prefix, 
        [Parameter(Mandatory = $true, Position = 2)] [String]
        $Suffix, 
        [Parameter(Position = 3)] [Boolean]
        $IgnoreAlreadyHave = $true,
        [Boolean]
        $ProcessDirectory = $true)
    $allItem = $(Get-ChildItem -Path $Path)
    $FILEATTRIBUTES = [System.IO.FileAttributes]
    foreach ($childItem in $allItem) {
        if ((($childItem.Attributes -eq $FILEATTRIBUTES::Directory) -and $ProcessDirectory) -or 
            ($childItem.Attributes -eq 
                ($childItem.Attributes -band ($FILEATTRIBUTES::Archive -bor 
                        $FILEATTRIBUTES::Encrypted -bor $FILEATTRIBUTES::Hidden -bor $FILEATTRIBUTES::Normal)))) {
            if (-not ($IgnoreAlreadyHave -and $childItem.BaseName.toString().StartsWith($Prefix))) {
                $destName = $Prefix + $childItem.BaseName
            }
            else {
                $destName = $childItem.BaseName
            }
            if (-not ($IgnoreAlreadyHave -and $childItem.BaseName.toString().EndsWith($Suffix))) {
                $destName += $Suffix
            }
            try {
                Move-Item -Path $childItem.FullName -Destination $($childItem.Directory.toString() + "/" + $destName + $childItem.Extension)
                Write-Output $("### Move <" + $childItem.FullName + ">")
            }
            catch {
                Write-Output $("!!! Move <" + $childItem.FullName + "> Failed.")
            }
        }
    }
}

Export-ModuleMember -Function @("Add-EXtraName")