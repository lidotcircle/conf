# Powershell Profile

# description: Change directory to desktop
#{ function : ddd()
function ddd()
{
    Set-Location $HOME/Desktop;
}
#}

# description: Reboot use `shutdown -r -t 0`
#{ function : rebootnow()
function rebootnow()
{
    shutdown -r -t 0;
}
#}

# description: Shutdown the computer with `shutdown -s -t 0`
#{ function : shutdownnow()
function shutdownnow()
{
    shutdown -s -t 0;
}
#}

# PowerShell ReadLine settings
#{ powershell readline

# For calling Static method in [MicroSoft.PowerShell.PSConsoleReadLine]
$consoleReadLine = [MicroSoft.PowerShell.PSConsoleReadLine]

# hashtables for key and handler
$keyHandler = @{
# Basic operation
    "ctrl+b"=[System.Management.Automation.ScriptBlock]{$consoleReadLine::BackwardChar()};
    "ctrl+f"=[System.Management.Automation.ScriptBlock]{$consoleReadLine::ForwardChar()};
    "alt+b"=[System.Management.Automation.ScriptBlock]{$consoleReadLine::BackwardWord()};
    "alt+f"=[System.Management.Automation.ScriptBlock]{$consoleReadLine::ForwardWord()};
    "ctrl+e"=[System.Management.Automation.ScriptBlock]{$consoleReadLine::EndOfLine()};
    "ctrl+a"=[System.Management.Automation.ScriptBlock]{$consoleReadLine::BeginningOfLine()};
# some others
    "alt+a" = [System.Management.Automation.ScriptBlock]{$consoleReadLine::SelectAll()};
    "alt+d" = [System.Management.Automation.ScriptBlock]{$consoleReadLine::RevertLine()};
    "ctrl+n" = [System.Management.Automation.ScriptBlock]{$consoleReadLine::NextHistory()};
    "ctrl+p" = [System.Management.Automation.ScriptBlock]{$consoleReadLine::PreviousHistory()};
    "alt+j" = [System.Management.Automation.ScriptBlock]{$consoleReadLine::Scrolldisplaydown()};
    "alt+k" = [System.Management.Automation.ScriptBlock]{$consoleReadLine::Scrolldisplayup()};
# automatic complete cmdlet
    "alt+c" = [System.Management.Automation.ScriptBlock]{$consoleReadLine::Complete()}
    "alt+m" = [System.Management.Automation.ScriptBlock]{$consoleReadLine::Complete()}
}


function ReadLineSettings()
{
    foreach($keyVal in $keyHandler.getEnumerator()){
        Set-PSReadLineKeyHandler -chord $keyVal.key -ScriptBlock $keyVal.Value
    }
}

while($true){
    ReadLineSettings;
    break;
}
#}
