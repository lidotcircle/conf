#!/bin/bash

# Description: 用于加入需要屏蔽的域名进入hosts文件中

SUCCESS=0
FAIL=1

if [ -d /tmp ]; then
    Temp_file=`mktemp /tmp/phosts.XXXXXX`
    Temp_file_uniq=`mktemp /tmp/phosts.XXXXXX`
else
    Temp_file=`mktemp`
    Temp_file_uniq=`mktemp`
fi

# 根据 terminal width 生成标记线
bbb=""
ddd=""
cols=$(tput cols)
for ((i=0;i<${cols};i++)); do
    bbb=${bbb}"*"
    ddd=${ddd}'='
done

HELP="\
${bbb}
Usage:
        phosts.sh [-sf <file>] [-tf <file>] [-tsf <file>] [-f <hosts-file>] [<site-address>]
        phosts.sh [-h | --help]

option:
        -sh             指定需要添加测试的网址文件[default -- none]
        -tf             指定输出测试成功的网址输出文件[default -- none]
        -tsf            指定直接添加的网址文件[default --none]
        -f              指定hosts文件[default -- /etc/hosts]
        -h | --help     输出帮助信息
        --output-on     打开添加网址的输出[default]
        --output-off    关闭添加网址的输出
    
Description:
        用于加入需要屏蔽的域名进入hosts文件中
${bbb}"

if [ -z ${1} ]; then
    echo "Need input parameter, error and exit."
    echo "${HELP}"
    exit ${FAIL}
else
    if [ ${1} = "-h" ] || [ ${1} = "--help" ]; then
        echo "${HELP}"
        exit ${SUCCESS}
    fi
fi

if !(ping -c 1 114.114.114.114 > /dev/null); then
    echo "Network error!"
    exit
fi

# put args to parameter array
count=0
for ii in ${*}; do
    parameter[count]=${ii}
    count=$[$count + 1]
done

# delete a element in parameter array
del_element_array() # delete element position -- ${1}
{
    del_pos=${1}
    pap_long=${#parameter[*]}
    pap_long=$[$pap_long - 1]
    if [ ${del_pos} \> ${pap_long} ]; then
        return ${FAIL}
    fi
    for ((i=${del_pos};i<${pap_long};i++)); do
        j=$[$i + 1]
        temp=${parameter[j]}
        parameter[i]=${temp}
    done
    unset parameter[pap_long]
}
del_element_array_2() # 和 del_element_array 功能相同
{
    del_pos=${1}
    temp=(${parameter[del_pos]})
    parameter=(${parameter[@]/$temp})
}

# 使用<Dest_file>表示'-f'选项,<TFile>表示'-ft‘选项,<SFile>表示'-sf'选项,<TSFile>表示'-tsf'选项
# 以on=[0|1]表示'--output-on','output-off'
while true; do
    echo "${ddd}"
    echo "Start......"
    echo "${ddd}"
    break
done
count=0
output=${SUCCESS}       # 默认开启输出
output_set_flags=0      # 用于指定flags, output-on output-off只出现一次
pat="[a-zA-Z0-9]{2,}[.]([a-zA-Z0-9]{2,})"     # 简单的匹配网址
pat2="(www|wap|bbs|m|)[.][a-z0-9A-Z]{2,}[.][a-zA-Z0-9]{2,}"        #匹配无需prefix的网址
while [ ${count} -le ${#parameter[*]} ]; do
    if [ -z ${parameter[count]} ]; then
        break
    fi
    # 指定<Dest_file> -- "-f"
    if [[ ${parameter[count]} == "-f" ]] && [ -z ${Dest_file} ]; then
        del_element_array_2 ${count}
        # 用于防止'-f','-sf','-tf','-tsf'为最后一个参数
        if [[ ${parameter[count]} == "" ]]; then
            echo '"-f"指定错误'
            exit ${FAIL}
        else
            Dest_file=${parameter[count]}
            touch ${Dest_file}
            del_element_array_2 ${count}
        fi
        continue
    fi
    # 指定<SFile> -- "-sf"
    if [[ ${parameter[count]} = "-sf" ]] && [ -z ${SFile} ]; then
        del_element_array_2 ${count}
        if [[ ${parameter[count]} == "" ]]; then
            echo '"-sf"指定错误'
            exit ${FAIL}
        else
            SFile=${parameter[count]}
            del_element_array_2 ${count}
        fi
        continue
    fi
    # 指定<TFile> -- "-tf"
    if [[ ${parameter[count]} == "-tf" ]] && [ -z ${TFile} ]; then
        del_element_array_2 ${count}
        if [[ ${parameter[count]} == "" ]]; then
            echo '"-tf"指定错误'
            exit ${FAIL}
        else
            TFile=${parameter[count]}
            touch ${TFile}
            del_element_array_2 ${count}
        fi
        continue
    fi
    # 指定<TSFile> -- "-tsf"
    if [[ ${parameter[count]} == "-tsf" ]] && [ -z ${TSFile} ]; then
        del_element_array_2 ${count}
        if [[ ${parameter[count]} == "" ]]; then
            echo '"-tsf"指定错误'
            exit ${FAIL}
        else
            TSFile=${parameter[count]}
            del_element_array_2 ${count}
        fi
        continue
    fi
    # 判定是否开启输出
    if [[ ${parameter[count]} == "--output-off" ]]; then
        if [ ${output_set_flags} -eq 1 ]; then
            echo "can set output flag twice, will use first time"
        else
            output=${FAIL}
        fi
        del_element_array_2 ${count}
        output_set_flags=1
        continue
    fi
    if [[ ${parameter[count]} == "--output-on" ]]; then
        if [ ${output_set_flags} -eq 1 ]; then
            echo "can set output flag twice, will use first time"
        fi       
        del_element_array_2 ${count}
        output_set_flags=1
        continue
    fi
    if [[ ${parameter[count]} =~ ${pat} ]]; then
        count=$[$count + 1]
        continue
    fi
    echo "unknow agrs \"${parameter[count]}\", delete it"
    del_element_array_2 ${count}
done

# the default file
## Dest_file
if [ -z ${Dest_file} ]; then
    Dest_file=/etc/hosts
fi
if [ -z ${SFile} ]; then
    SFile=None
fi
if [ -z ${TFile} ]; then
    TFile=None
fi
if [ -z ${TSFile} ]; then
    TSFile=None
fi

# OUTPUT OPTION
while true; do
    echo "*************"
    echo "INFO CHECK! *"
    echo "${bbb}"
    echo "The '-f' option choose \"${Dest_file}\" file"
    echo "The '-tf' option choose \"${TFile}\" file"
    echo "The '-sf' option choose \"${SFile}\" file"
    echo "The '-tsf' option choose \"${TSFile}\" file"
    if [ ${output} -eq ${SUCCESS} ]; then
        echo "The stdout is on"
    else
        echo "The stdout is off"
    fi
    echo "The stdin will add ${#parameter[*]} times"
    echo "${bbb}"
    echo "Sure?[Y/N]:"
    while true; do
        shopt -s nocaseglob
        read chos;
        if [[ ${chos} =~ "n" ]] || [[ ${chos} =~ "no" ]]; then
            echo "You sad \"${chos}\", and it will exit, please try agian."
            exit ${FAIL}
        elif [[ ${chos} == "" ]] || [[ ${chos} =~ "y" ]] || [[ ${chos} =~ "yes" ]]; then
            echo "You sad \"${chos}\", and it will start."
            echo "${ddd}"
            break
        else
            echo "You should correctly input the option, try again[Y/N]:"
        fi
    done
    break
done

echo "Begin......"

prefix_set=("www." "bbs." "wap." "m.")

if [ ${output} -eq ${FAIL} ]; then
    exec 6>&1
    exec 1>/dev/null
fi

# Add site from <TSFile>
if [ -f ${TSFile} ]; then
    counter=1
    while [ ! $(sed -n "${counter} p" ${TSFile}) = "" ];do
        echo -e "127.0.0.1\t$(sed -n "${counter} p" ${TSFile})" >> ${Temp_file}
        counter=$[$counter + 1]
    done
fi

# prefix flag
prefix_flag=0
# Add Site Function
add_site()
{
    echo "Test ${1}."
    if !(grep "127.0.0.1.*${1}\$" ${Temp_file} ${Dest_file} >> /dev/null); then
        if (ping -c 1 ${1} >> /dev/null); then
            if [ ! ${TFile} = "None" ]; then
                if !(grep "^${1}" ${TFile}); then
                    echo ${1} >> ${TFile}
                fi
            fi
            echo -e "127.0.0.1\t${1}" >> ${Temp_file}
            echo "Add ${1}."
        fi
    fi
    if [[ ${prefix_flag} == 0 ]]; then
        for site_prefix in ${prefix_set[*]}; do
            echo "Test ${site_prefix}${1}."
            if !(grep "127.0.0.1.*${site_prefix}${1}\$" ${Temp_file} ${Dest_file} >> /dev/null); then
                if (ping -c 1 ${site_prefix}${1} >> /dev/null); then
                    if [ ! ${TFile} = "None" ]; then
                        if !(grep "^${site_prefix}${1}" ${TFile}); then
                            echo ${site_prefix}${1} >> ${TFile}
                        fi
                    fi
                    echo -e "127.0.0.1\t${site_prefix}${1}" >> ${Temp_file}
                    echo "Add ${site_prefix}${1}."
                fi
            fi
        done
    fi
}

# Add Site Function with file
add_site_file()
{
    count_total=`wc -l ${1} | grep -o '[0-9]*'`
    for (( i=1; i <= ${count_total}; i++ )); do
        site_temp=`sed -n "${i} p" ${1}`
        if [[ ${site_temp}} =~ ${pat} ]]; then
            if [[ ${site_temp} =~ ${pat2} ]]; then
                prefix_flag=1
            else
                prefix_flag=0
            fi
            add_site ${site_temp}
        fi
    done
}

# MAIN PART
while true; do
    # add frome <SFile>
    if [ -f ${SFile} ]; then
        add_site_file ${SFile}
    fi
    # add from parameter
    for site in ${parameter[*]}; do
        if [[ ${site} =~ ${pat2} ]]; then
            prefix_flag=1
        else
            prefix_flag=0
        fi
        add_site ${site}
    done
    break
done

if [ ${output} -eq ${FAIL} ]; then
    exec 1>&6
fi

if [ -f ${Temp_file} ]; then
    sort ${Temp_file} | uniq >> ${Temp_file_uniq}
    cat ${Temp_file_uniq} >> ${Dest_file}
else
    echo "Can\'t add any hosts"
    exit ${FAIL}
fi

echo "Finish!"
exit ${SUCCESS}
