#!/bin/bash

[ -z $(which curl) ] && echo "need curl"

declare -r SSR_SUB=$(uci get ssr_json.@ssrsub[0].suburl)
declare -r SUB_FILE=$(uci get ssr_json.@ssrsub[0].subfile)
declare -r SUB_FILE_DST="$SUB_FILE-dst"
echo "" > ${SUB_FILE_DST}
declare -r TMP_FILE=$(mktemp)
declare -r LOG__=/var/log/subssr.log
touch ${LOG__}
declare -r SSR_CONF_JSON="/etc/shadowsocksr.json"
# ps to show the process list that contain ssr, and first column is pid.
declare -r PS=ps
# ssr program
declare -r SSR_PID="/var/run/ssr-redir-go.pid"
declare -r SSR_REDIR=$(which ssr-redir || echo "/usr/sbin/ssr-redir")
declare -r SSR_START="$SSR_REDIR -u -c ${SSR_CONF_JSON} -f ${SSR_PID} 1>> ${LOG__} 2>&1"

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
    ping -w 5 -c 2 $1 || \
        (echo -e "### CAN'T access server <$1> ###\n" >> ${LOG__} && return 1)
    return 0
}
#}

#{ function: add_node()
declare -r TMP_FILEB=$(mktemp)
declare -i ssr_node_i=0
add_node()
{
    echo "$1" | base64 -d 1> ${TMP_FILEB} || return 1
    local first_part=$(cat ${TMP_FILEB} | cut -f 1 -d\/)
    local server=$(echo "${first_part}" | cut -f 1 -d:)
    local server_port=$(echo "${first_part}" | cut -f 2 -d:)
    test_ssr_server "${server}" "${server_port}" || return 1
    echo "### adding node <${server}> ###" >> ${LOG__}
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
    for((__loop=1;$__loop<=$para_vary;__loop++)); do
        para=$(echo "$second_part" | cut -f $__loop -d\&)
        para_name=$(echo "$para" | cut -f 1 -d=)
        para_cont=$(echo "$para" | cut -f 2 -d=)
        eval "tmp_ref[\"${para_name}\"]=\"${para_cont}\""
    done
    declare -p ssr_node_${ssr_node_i} >> ${LOG__}
    let ssr_node_i++
    echo -e "############################################\n\n" >> ${LOG__}
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
    [ -n ${ssrpid} ] && kill -9 ${ssrpid} && echo -e "### kill ssr process at <$(date)> ###" >> ${LOG__}
    [ -f ${SSR_CONF_JSON} ] && rm -f ${SSR_CONF_JSON} && \
        echo "remove ssr config file -- ${SSR_CONF_JSON}" >> ${LOG__}
    local rand=$(($RANDOM%$ssr_node_i))
    [ -n "$1" ] && rand=$1
    rm -f ${LUCI_SSRPRO}; luci_ssrpro_config "${rand}" "${LUCI_SSRPRO}"
    print_json "${rand}" "${SSR_CONF_JSON}"
    echo "restart ssr." >> ${LOG__} && ${SSR_START} && echo "start ssr success." >> ${LOG__}
}
#}

#{ function: check_accessibility()
check_accessibility()
{
    curl --max-time 5 https://google.com || \
        (echo "Can't access google... [$(date)]" >> ${LOG__} && return 1) && \
        (echo "Google is accessible... [$(date)]" >> ${LOG__} && return 0)
}
#}

# banner
echo -e "\n########## [$(date)] ##########\n" >> ${LOG__}

declare -i supr_stdout
get_fd supr_stdout && eval "exec $supr_stdout>&1 1>/dev/null"
declare -i supr_stderr
get_fd supr_stderr && eval "exec $supr_stderr>&2 2>/dev/null"

check_accessibility && exit 0

# get the subscription file from URL
curl --max-time 5 ${SSR_SUB} -o ${TMP_FILE} && cp -f ${TMP_FILE} ${SUB_FILE} \
    || ([ -f ${SUB_FILE}] && cp -f ${SUB_FILE} ${TMP_FILE})
[ ! -f $SUB_FILE ] && echo "subscription fail!!!" >> ${LOG__} && exit 1

# decode subscription file
base64 -d ${TMP_FILE} > ${SUB_FILE_DST}
cat ${SUB_FILE_DST} | grep -oe "[^:^/]\{4,\}" > ${TMP_FILE}

# add node to hash table
declare -i node_all_i=$(cat ${TMP_FILE} | wc -l)
for ((node_loop=1;$node_loop<=$node_all_i;node_loop++)); do
    __node=$(cat ${TMP_FILE} | head -n $node_loop | tail -n 1)
    add_node "${__node}" "${SUB_FILE_DST}" 2>> /dev/null
done

declare -i max_try=10
for((i=0;$i<$max_try;i++)); do
    check_accessibility && exit 0 || restart_ssr
    sleep 20
done
