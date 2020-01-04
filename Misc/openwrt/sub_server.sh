#!/bin/bash

# 0.     check prerequisite software.
# 1.     if network is accessible, then nothing will be do.
# 2.     otherwise.
# 2.1.     try to fetch subscription.
# 2.1.1.     if fetch subscription fail, using previous cache
# 2.1.2.     if previous cache isn't avaliable, kill this process
# 2.2.     updating shadowsocksr files by decoding subscription file
# 2.3.     if existing a shadowsocksr process, kill it
# 2.4.     restart shadowsockrsr process with new server, 
#          and redo this step until can connect to <https times://google.com> with 
#          interval of 20 seconds and maxmimum trying of 5 times.

# file mutex
declare -r EXCLUSIVE_PROCESS="/var/run/sub_server_sh.mutex"
if [ -e ${EXCLUSIVE_PROCESS} ]; then
    the_modification_of_previous_mutex_file=$(date -r ${EXCLUSIVE_PROCESS} +%s)
    current_time=$(date +%s)
    difference_time=$((current_time - the_modification_of_previous_mutex_file))
    if [ ${difference_time} -gt 600 ]; then
        rm -rf ${EXCLUSIVE_PROCESS}
    else
        exit 0
    fi
fi
echo -e "$$" > ${EXCLUSIVE_PROCESS}

#{ logger
__logger__()
{
    local MSG
    local PRIORITY
    if [ $# -eq 1 ]; then
        MSG="$1"
        PRIORITY="INFO"
    else
        PRIORITY="$1"
        MSG="$2"
    fi
    if [ "${PRIORITY}" == "BANNER" ]; then
        echo -e "$MSG"
        return 0
    fi
    if [ "${PRIORITY}" == "ERROR" ]; then
        echo -e "ERROR [$(date)]: $MSG" 1>&2
        return 0
    else
        echo -e "${PRIORITY} [$(date)]: $MSG"
        return 0
    fi
} #}

# dependencies check
[ -z $(which curl) ] && __logger__ "ERROR" "need curl" && exit 1
[ -z $(which uci)  ] && __logger__ "ERROR" "need uci"  && exit 1

declare -r SSR_SUB_DIR="/etc/ssr_fold__"
declare -r SSR_SUB=$(uci get ssr_json.@ssrsub[0].suburl)
declare -r SUB_FILE=$(uci get ssr_json.@ssrsub[0].subfile)
if [ -d ${SUB_FILE%/*} ]; then
    mkdir -p ${SUB_FILE%/*}
fi
[ -f ${SUB_FILE} ] && echo "" > ${SUB_FILE} || touch ${SUB_FILE}

declare -r SUB_FILE_DST="$SUB_FILE-dst"
[ -f ${SUB_FILE_DST} ] && echo "" > ${SUB_FILE_DST} || touch ${SUB_FILE_DST}

declare -r TMP_FILE=$(mktemp)

# redirect log
declare -r INF_LOG="/var/log/subssr.log"
declare -r ERR_LOG="/var/log/subssr_err.log"
exec 1<> ${INF_LOG}
exec 2<> ${ERR_LOG}

declare -r SSR_CONF_JSON="/etc/shadowsocksr.json"

# ps to show the process list that contain ssr, and first column is pid.
declare -r PS=ps

# ssr program
declare -r SSR_PID="/var/run/ssr-redir-go.pid"
declare -r SSR_REDIR=$(which ssr-redir || echo "/usr/sbin/ssr-redir")
declare -r SSR_START="$SSR_REDIR -u -c ${SSR_CONF_JSON} -f ${SSR_PID}"

declare -i NMAP_AVAIL=0
declare -i NETCAT_AVAIL=0
declare -i PING_AVAIL=0

declare NETCAT_CMD="netcat"

[ -n "$(which nmap)" ]               && NMAP_AVAIL=1
[ -n "$(which nc || which netcat)" ] && NETCAT_AVAIL=1
[ -n "$(which ping)" ]               && PING_AVAIL=1
while(true); do
if [ $NETCAT_AVAIL -eq 1 ]; then
    [ -n "$(which netcat)" ] && break
    NETCAT_CMD="nc" && break
fi
done

 #{ function: clean_exit(exit_status)
clean_exit()
{
    __logger__ "function clean_exit() is called."
    [ -f "${TMP_FILE}"  ]         && rm -f ${TMP_FILE}
    [ -f "${TMP_FILEB}" ]         && rm -f ${TMP_FILEB}
    [ -f "${SUB_FILE}"  ]         && rm -f ${SUB_FILE}
    [ -f "${SUB_FILE_DST}" ]      && rm -f ${SUB_FILE_DST}
    [ -f "${EXCLUSIVE_PROCESS}" ] && rm -f ${EXCLUSIVE_PROCESS}
    [ "${START_BANNER}" == 1 ] && __logger__ "BANNER" "----------------- END $$ ------------------\n"
    [ $# -eq 1 ] && exit $1
    exit 1
} #}

#{ function: get_fd() <ret_ref> -- aviliable file descriptor
get_fd()
{
    __logger__ "function get_fd() is called."
    [ $# -eq 1 ] || return 1
    local -n __found=$1
    __found=-1
    for fd in {0..200}; do
        rco="$(true 2>/dev/null >&${fd}; echo $?)"
        rci="$(true 2>/dev/null <&${fd}; echo $?)"
        [[ "${rco}${rci}" = "11" ]] && __found=${fd} && break
    done
    [ $__found -eq -1 ] && return 1
    return 0
}
#}

#{ function: test_ssr_server()
test_ssr_server()
{
    __logger__ "function test_ssr_server() is called."
    # just one argument passed
    if [ $# -eq 1 ]; then
        [ $PING_AVAIL -eq 1 ] && ping -w 5 -c 2 $1 || \
            (__logger__ "WARNING" "CAN'T access server <$1>" && return 1)
        return 0
    fi
    if [ $# -eq 2 ]; then
        if [ $NMAP_AVAIL -eq 1 ]; then
            nmap --host-timeout 2 $1 -p $2 2>/dev/null | grep -ie "OPEN" 1>/dev/null 2>&1 && return 0
            __logger__ "WARNING" "CAN'T access server <$1:$2>" && return 1
        fi
        if [ $NETCAT_AVAIL -eq 1 ]; then
            $NETCAT_CMD -zvw2 $1 $2 1>/dev/null 2>&1 && return 0
            __logger__ "WARNING" "CAN'T access server <$1:$2>" && return 1
        fi
        if [ $PING_AVAIL -eq 1 ]; then
            ping -w 5 -c 2 $1 1>/dev/null 2>&1 && return 0
            __logger__ "WARNING" "CAN'T access server <$1>" && return 1
        fi
    fi
    __logger__ "INFO" "unable to test accessibility of server, just return true." && return 0
}
#}

#{ function: add_node()
declare -r TMP_FILEB=$(mktemp)
declare -i ssr_node_i=0
add_node()
{
    __logger__ "function add_node() is called."
    echo "$1" | base64 -d 1> ${TMP_FILEB} 2>/dev/null
    [ -z $(cat ${TMP_FILEB}) ] && return 1
    local first_part=$(cat ${TMP_FILEB} | cut -f 1 -d\/)
    local server="$(echo "${first_part}" | cut -f 1 -d:)"
    local server_port=$(echo "${first_part}" | cut -f 2 -d:)
    test_ssr_server "${server}" "${server_port}" || return 1
    local second_part=$(cat ${TMP_FILEB} | cut -f 2 -d?)
    __logger__ "INFO" "---------- Add Node ---------"
    eval "declare -g -A ssr_node_${ssr_node_i}"
    declare -n tmp_ref=ssr_node_${ssr_node_i}
    # First part
    tmp_ref["server"]=$server
    tmp_ref["server_port"]=$server_port
    tmp_ref["protocol"]=$(echo "${first_part}" | cut -f 3 -d:)
    tmp_ref["method"]=$(echo "${first_part}" | cut -f 4 -d:)
    tmp_ref["obfs"]=$(echo "${first_part}" | cut -f 5 -d:)
    tmp_ref["password"]=$(echo "${first_part}" | cut -f 6 -d: | base64 -d)
    # second part
    local -i para_vary=$(echo "$second_part" | grep -oe "\&" | wc -l)
    let para_vary++
    local -i __loop
    local para
    local para_name
    local para_cont
    local para_cccc
    for((__loop=1;$__loop<=$para_vary;__loop++)); do
        para=$(echo "$second_part" | cut -f $__loop -d\&)
        para_name=$(echo "$para" | cut -f 1 -d=)
        para_cont=$(echo "$para" | cut -f 2 -d=)
        echo $para_cont | base64 -d 1>/dev/null 2>&1 
        # FIXXX
        if [ $? -eq 0 ] || [ $para_name == "obfsparam" ] || [ $para_name == "protoparam" ]; then
            para_cont=$(echo $para_cont | base64 -d 2>/dev/null)
        fi
        eval "tmp_ref[\"${para_name}\"]=\"${para_cont}\""
    done
    declare -p ssr_node_${ssr_node_i}
    let ssr_node_i++
}
#}

#{ function: print_json()
declare -ri RUNNING_PORT=7070
print_json()
{
    __logger__ "function print_json() is called."
    echo "{" >> $2
    declare -n tmp_ref=ssr_node_$1
    __logger__ "INFO" "print json of <server: ${tmp_ref["server"]}>"
    for __sym in ${!tmp_ref[@]}; do
        eval "value=\${tmp_ref[\"$__sym\"]}"
        if [[ "${value}" =~ ^[0-9]*$ ]] && [ -n "${value}" ]; then
            echo -e "    \"${__sym}\" : ${value}," >> $2
        else
            echo -e "    \"${__sym}\" : \"${value}\"," >> $2
        fi
    done
    local node_info_tail="    \"local_address\" : \"0.0.0.0\",
    \"local_port\" : ${RUNNING_PORT},
    \"time_out\" : 60,
    \"fast_open\" : false
}"
    echo "${node_info_tail}" >> $2
}
#}

#{ function: luci_ssrpro_config()
LUCI_SSRPRO="/etc/config/ssrpro"
default_options="
config ssrpro
    option gfwlist 'china-banned'
    option proxy_mode 'M'
    option safe_dns_tcp '1'
    option cron_mode '1'
    option more '0'
    option enabled '1'"
luci_ssrpro_config()
{
    __logger__ "function luci_ssrpro_config() is called."
    echo "${default_options}" >> "$2"
    declare -n tmp_ref=ssr_node_$1
    __logger__ "INFO" "print luci configuration of <server: ${tmp_ref["server"]}>"
    for __sym in ${!tmp_ref[@]}; do
        eval "value=\${tmp_ref[\"$__sym\"]}"
        echo "    option ${__sym} '${value}'" >> $2
    done
}
#}

#{ function: write_to_fold() -- write nodes into files in specified fold
write_to_fold()
{
    __logger__ "function write_to_fold() is called."
    [ ! $# -eq 1 ] && return 1
    if [ ! -d "$1" ]; then
        if [ ! -e $1 ]; then
            mkdir -p $1
        else
            rm -f $1
            mkdir -p $1
        fi
        __logger__ "INFO" "create fold <$1>"
    else
        rm -rf "$1"
        mkdir $1
        __logger__ "INFO" "clean fold <$1>"
    fi

    __logger__ "amount of available server is $ssr_node_i"
    for ((i=1; i<=$ssr_node_i; ++i)); do
        print_json "$i" "$1/json_$i"
        luci_ssrpro_config "$i" "$1/conf_$i"
    done
} #}

#{ function: start_ssr_process() -- as the function name suggests
start_ssr_process()
{
    __logger__ "function start_ssr_process() is called."
    [ ! $# -eq 1 ] && __logger__ "ERROR" "Invaliated Argument at FUNCTION start_ssr_process()" && clean_exit 1

    [ -f ${SSR_CONF_JSON} ] && rm -f ${SSR_CONF_JSON} && \
        __logger__ "INFO" "remove ssr json config file -- ${SSR_CONF_JSON}"
    [ -f ${LUCI_SSRPRO}   ] && rm -f ${LUCI_SSRPRO}   && \
        __logger__ "INFO" "remove luci config file -- ${LUCI_SSRPRO}"

    cp -f "${SSR_SUB_DIR}/json_$1" ${SSR_CONF_JSON}
    cp -f "${SSR_SUB_DIR}/conf_$1" ${LUCI_SSRPRO}
    __logger__ "INFO" "To start ssr with server $1"
    ${SSR_START} && __logger__ "INFO" "start ssr success, \n           command line: [${SSR_START}]"
    return 0
}
#}

#{ function: kill_ssr_process()
kill_ssr_process()
{
    __logger__ "function kill_ssr_process() is called."
    [ -f ${SSR_PID} ] && rm -rf ${SSR_PID}
    local -i ssrpid=0
    local IFS=$'\n'
    ssrprocess=$(ps | grep "ssr-redir" | grep -v "grep")
    for ssrpcs in ${ssrprocess}; do
        ssrpid=0
        ssrpid=$(echo ${ssrpcs} | cut -d\  -f 1)
        if [ $ssrpid -eq 0 ]; then
            ssrpid=$(echo ${ssrpcs} | cut -d\  -f 2)
        fi
        kill -9 $ssrpid
        __logger__ "INFO" "kill shadowsocksr process #${ssrpid} ###"
    done
}
#}

#{ function: restart_ssr()
RANDOM=$$
restart_ssr()
{
    __logger__ "function restart_ssr() is called."
    # amount of server that is available
    declare -ri count_of_server=$(ls ${SSR_SUB_DIR} | grep "json" | wc -l)
    [ ${count_of_server} -eq 0 ] && __logger__ "ERROR" "none of server is available." && clean_exit 1
    __logger__ "INFO" "total amount of server that is available is ${count_of_server}"

    kill_ssr_process

    local -i rand=$(($RANDOM%$count_of_server))
    [ -n "$1" ] && rand=$1
    let rand++

    start_ssr_process "${rand}"
}
#}

#{ function: check_accessibility()
check_accessibility()
{
    __logger__ "function check_accessibility() is called."
    curl --max-time 5 https://google.com 1>/dev/null 2>&1
    if [ $? -eq 0 ]; then
        __logger__ "INFO" "google is accessible" && return 0
    else
        __logger__ "INFO" "can't reach https://google.com" && return 1
    fi
}
#}

# banner
__logger__ "BANNER" "\n--------------- START $$ -------------------"
declare -r START_BANNER="1"

check_accessibility && clean_exit 0

# get the subscription file from URL
declare -i SUBSCRIPTION_SUCCESS=1
__logger__ "INFO" "Try to update subscription, [$SSR_SUB]"
curl --max-time 5 ${SSR_SUB} -o ${TMP_FILE} 1>>/dev/null 2>&1 && cp -f ${TMP_FILE} ${SUB_FILE} \
    || ([ -f ${SUB_FILE} ] && cp -f ${SUB_FILE} ${TMP_FILE})
[ ! -f $SUB_FILE ] && __logger__ "WARNING" "subscription fail!!!"  && SUBSCRIPTION_SUCCESS=0

if [ "$(cat ${TMP_FILE} | wc -l)" == "0" ]; then
    __logger__ "WARNING" "subscription fail, empty."
    SUBSCRIPTION_SUCCESS=0
fi


while [ $SUBSCRIPTION_SUCCESS -eq 1 ]; do
__logger__ "INFO" "subscription success"

# decode subscription file
base64 -d ${TMP_FILE} > ${SUB_FILE_DST} 2>/dev/null
cat ${SUB_FILE_DST} | grep -oe "[^:^/]\{4,\}" > ${TMP_FILE}

# add node to hash table
declare -i node_all_i=$(cat ${TMP_FILE} | wc -l)
__logger__ "INFO" "count of lines of subscription file is ${node_all_i}"
for ((node_loop=1;$node_loop<=$node_all_i;node_loop++)); do
    __node=$(cat ${TMP_FILE} | head -n $node_loop | tail -n 1)
    add_node "${__node}" "${SUB_FILE_DST}"
done

# write write write ...
write_to_fold "${SSR_SUB_DIR}"

break # while [ $SUBSCRIPTION_SUCCESS -eq 1];
done


declare -i max_try=5
for((i=0;$i<$max_try;i++)); do
    check_accessibility && clean_exit 0 || restart_ssr
    sleep 20
done

clean_exit 1
