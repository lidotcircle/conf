#!/usr/bin/env bash

USAGE()
{
    cat <<EOF
Usage:
        nncc.sh -s <file-s> -r <file-r> -l <listen port> -c <dst server>
        
        -s <file-s>      file that send from cilent to server
        -r <file-r>      file that send from server to cilent
        -l <listen port> listening this port
        -c <dst server>  the host:port combination that user want to connnect
EOF
}

[ $# -lt 4 ] && USAGE && exit 1

declare S_FILE=""
declare R_FILE=""
declare L_PORT=""
declare DST_HOST=""
declare DST_PORT=""

# var_name, value, pattern
assign_what_empty()
{
    [ $# -eq 3 ] || (echo "debug here" && exit 2)
    local -n vn=$1
    [ -n "$vn" ] && USAGE && exit 3
    vn="$2"
    [ -z "$(echo "$2" || grep -se $3)" ] && USAGE && exit 4
}

while [ $# -ge 1 ]; do
    case $1 in
        -s) assign_what_empty S_FILE $2 '^.\+$'
            shift 2
            continue;;
        -r) assign_what_empty R_FILE $2 '^.\+$'
            shift 2
            continue;;
        -l) assign_what_empty L_PORT $2 '^[[:digit:]]\{2,5\}$'
            shift 2
            continue;;
        -c) assign_what_empty DST_HOST "$(echo $2 | grep -oe '^[^:]*')" '^.*$'
            assign_what_empty DST_PORT "$(echo $2 | grep -oe ':.*$' | grep -o '[^:]*$')" '^.*$'
            shift 2
            continue;;
        -*) USAGE && exit 5
            ;;
    esac
    if [ -z "$S_FILE" ]; then
        assign_what_empty S_FILE $1 '^.\+$'
    elif [ -z "$R_FILE" ]; then
        assign_what_empty R_FILE $1 '^.\+$'
    elif [ -z "$L_PORT" ]; then
        assign_what_empty L_PORT $1 '^[[:digit:]]\{2,5\}$'
    elif [ -z "$DST_PORT" ]; then
        assign_what_empty DST_HOST "$(echo $1 | grep -oe '^[^:]*')" '^.*$'
        assign_what_empty DST_PORT "$(echo $1 | grep -oe ':.*$' | grep -o '[^:]*$')" '^.*$'
    else
        USAGE && exit 6
    fi
    shift 1
done

declare NC=
[ ! -x "$(which nc)" ] && [ ! -x "$(which netcat)" ] && echo "need nc or netcat" && exit 7
[ ! -x "$(which nc)" ] && NC="nc" || NC="netcat"

declare send___=$(mktemp -u)
mkfifo -m 600 "$send___"
declare recieve=$(mktemp -u)
mkfifo -m 600 "$recieve"

clear_exit()
{
    [ $# -eq 1 ] || exit 8
    [ -f $send___ ] && rm -f $send___
    [ -f $recieve ] && rm -f $recieve
    exit $1
}

$recieve | $NC -l L_PORT | tee $send___ > $S_FILE &
declare LS=$?
declare LP=$!
if [ ! $LS -eq 0 ]; then
    echo "nc listening fail with $LS"
    clear_exit $LS
fi

cat $send___ | $NC $DST_HOST $DST_PORT | tee $recieve > $R_FILE
LS=$?
if [ ! $LS -eq 0 ]; then
    echo "nc connecting fail with $LS"
    kill -9 ${LP}
    clear_exit $LS
fi

clear_exit 0
