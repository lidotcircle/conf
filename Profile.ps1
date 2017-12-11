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
