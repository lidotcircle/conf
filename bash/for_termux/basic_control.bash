# FileName: basic_control.bash
# Descript: control termux sessions

# rename current session
function rns()
{
    [ -n ${@} ] && printf "\033]2${@}\007"
    return
}
