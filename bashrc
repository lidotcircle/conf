#!/usr/bin/env bash

# 加载配置文件
## log file
ERROR_LOG=$HOME/.bashrc_error_log
## file list
soc_lis=("$HOME/.bash/alias" \
    "$HOME/.bash/env" \
    "$HOME/.extra_bashrc")

# Usage : error_handle <message>
#{ function : error_handle()
error_handle()
{
    [ $# -eq 0 ] && (echo "**CALL** Error handle fail." >> $ERROR_LOG && return 0) || \
        (echo "[$(date "+%m-%d %H:%M:%S")]:**WARNING** : $@" \
        >> $ERROR_LOG && return 0)
}
#}

# Usage : try_source_file <source_fil>
#{ function : try_source_fil()
try_source_fil()
{
    [ $# -eq 1 ] || (error_handle "Invalid parameter in try_source_fil()." && return 1)
    if [ -f $1 ]; then
        source $1 && return 0 || (error_handle "source file <$1> failed." && return 1)
    else
        error_handle "File <$1> don't exist." && return 1
    fi
}
#}

# Main
#{ Main
while(true); do
    for soc_fil in ${soc_lis[@]}; do
        try_source_fil $soc_fil
    done
    break
done
#}
