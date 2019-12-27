#!/bin/bash

# dependencies check
[ -z $(which curl) ] && echo "need curl" && exit 1
[ -z $(which uci)  ] && echo "need uci" && exit 1

declare -r SSR_SUB=$(uci get ssr_json.@ssrsub[0].suburl)
declare -r SUB_FILE=$(uci get ssr_json.@ssrsub[0].subfile)
if [ -d ${SUB_FILE%/*} ]; then
    mkdir -p ${SUB_FILE%/*}
fi
[ -f ${SUB_FILE} ] && truncate --size 0k ${SUB_FILE} || touch ${SUB_FILE}

declare -r SUB_FILE_DST="$SUB_FILE-dst"
[ -f ${SUB_FILE_DST} ] && truncate --size 0k ${SUB_FILE_DST} || touch ${SUB_FILE_DST}

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

#{ function: get_fd() <ret_ref> -- aviliable file descriptor
get_fd()
{
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
    # just one argument passed
    if [ $# -eq 1 ]; then
        [ $PING_AVAIL -eq 1 ] && ping -w 5 -c 2 $1 || \
            (echo -e "### CAN'T access server <$1> ###\n" && return 1)
        return 0
    fi
    if [ $# -eq 2 ]; then
        if [ $NMAP_AVAIL -eq 1 ]; then
           nmap --host-timeout 2 $1 -p $2 2>/dev/null | grep -ie "OPEN" 1>/dev/null 2>&1 && return 0
           echo -e "### CAN'T access server <$1:$2> ###\n" >&2 && return 1
        fi
        if [ $NETCAT_AVAIL -eq 1 ]; then
            $NETCAT_CMD -zvw2 $1 $2 1>/dev/null 2>&1 && return 0
            echo -e "### CAN'T access server <$1:$2> ###\n" >&2 && return 1
        fi
        if [ $PING_AVAIL -eq 1 ]; then
            ping -w 5 -c 2 $1 1>/dev/null 2>&1 && return 0
            echo -e "### CAN'T access server <$1> ###\n" >&2 && return 1
        fi
    fi
    echo -e "### unable to test accessibility of server, just return true." && return 0
}
#}

#{ function: add_node()
declare -r TMP_FILEB=$(mktemp)
declare -i ssr_node_i=0
add_node()
{
    echo "$1" | base64 -d 1> ${TMP_FILEB} 2>/dev/null
    [ -z $(cat ${TMP_FILEB}) ] && return 1
    local first_part=$(cat ${TMP_FILEB} | cut -f 1 -d\/)
    local server=$(echo "${first_part}" | cut -f 1 -d:)
    local server_port=$(echo "${first_part}" | cut -f 2 -d:)
    echo "${server}"
    test_ssr_server "${server}" "${server_port}" || return 1
    echo "### adding node <${server}> ###"
    local second_part=$(cat ${TMP_FILEB} | cut -f 2 -d?)
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
        echo "$para_name"
        eval "tmp_ref[\"${para_name}\"]=\"${para_cont}\""
    done
    declare -p ssr_node_${ssr_node_i}
    let ssr_node_i++
    echo -e "############################################\n\n"
}
#}

#{ function: print_json()
print_json()
{
    echo "{" >> $2
    declare -n tmp_ref=ssr_node_$1
    for __sym in ${!tmp_ref[@]}; do
        eval "value=\${tmp_ref[\"$__sym\"]}"
        if [[ "${value}" =~ ^[0-9]*$ ]] && [ -n "${value}" ]; then
            echo -e "    \"${__sym}\" : ${value}," >> $2
        else
            echo -e "    \"${__sym}\" : \"${value}\"," >> $2
        fi
    done
    local node_info_tail="    \"local_address\" : \"0.0.0.0\",
    \"local_port\" : 7070,
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
    echo "${default_options}" >> "$2"
    declare -n tmp_ref=ssr_node_$1
    for __sym in ${!tmp_ref[@]}; do
        eval "value=\${tmp_ref[\"$__sym\"]}"
        echo "    option ${__sym} '${value}'" >> $2
    done
}
#}

#{ function: restart_ssr()
RANDOM=$$
restart_ssr()
{
    local ssrpid
    if [ -f ${SSR_PID} ]; then
        ssrpid=$(cat ${SSR_PID})
    else
        ssrpid=$(${PS} | grep -se "ssr" | tail -n 1 | cut -f 1)
    fi
    [ -n ${ssrpid} ] && kill -9 ${ssrpid} && echo -e "### kill ssr process at <$(date)> ###"
    [ -f ${SSR_CONF_JSON} ] && rm -f ${SSR_CONF_JSON} && \
        echo "remove ssr config file -- ${SSR_CONF_JSON}"
    local rand=$(($RANDOM%$ssr_node_i))
    [ -n "$1" ] && rand=$1
    rm -f ${LUCI_SSRPRO}; luci_ssrpro_config "${rand}" "${LUCI_SSRPRO}"
    print_json "${rand}" "${SSR_CONF_JSON}"
    echo "restart ssr." && ${SSR_START} && echo "start ssr success."
}
#}

#{ function: check_accessibility()
check_accessibility()
{
    curl --max-time 5 https://google.com 1>/dev/null 2>&1 || \
        (echo "Can't access google... [$(date)]" && return 1) && \
        (echo "Google is accessible... [$(date)]" && return 0)
}
#}

# banner
echo -e "\n########## [$(date)] ##########\n"

check_accessibility && exit 0

# get the subscription file from URL
curl --max-time 5 ${SSR_SUB} -o ${TMP_FILE} 1>>/dev/null 2>&1 && cp -f ${TMP_FILE} ${SUB_FILE} \
    || ([ -f ${SUB_FILE}] && cp -f ${SUB_FILE} ${TMP_FILE})
[ ! -f $SUB_FILE ] && echo "subscription fail!!!"  && exit 1

# decode subscription file
base64 -d ${TMP_FILE} > ${SUB_FILE_DST} 2>/dev/null
cat ${SUB_FILE_DST} | grep -oe "[^:^/]\{4,\}" > ${TMP_FILE}

# add node to hash table
declare -i node_all_i=$(cat ${TMP_FILE} | wc -l)
for ((node_loop=1;$node_loop<=$node_all_i;node_loop++)); do
    __node=$(cat ${TMP_FILE} | head -n $node_loop | tail -n 1)
    add_node "${__node}" "${SUB_FILE_DST}"
done

declare -i max_try=10
for((i=0;$i<$max_try;i++)); do
    check_accessibility && exit 0 || restart_ssr
    sleep 20
done
