#!/usr/bin/env bash

## Exit status
EXIT_SUCCESS=0
EXIT_FAIL=1

HELP="
Description:
        用于抓取\"m.wanben.me\"部分小说的脚本.

Usage:
        catchwbm.sh {<site-info>} [<output-file>]
        catchwbm.sh {--help | -h}

Site Format:
        [[http://]m.wanben.me/xiaoshuo/]<number>/<number>[/]
Output:
        不指定输出文件名将使用小说名
"

## Help display
if [[ ${1} =~ "-h" ]] || [[ ${1} =~ "--help" ]]; then
    echo "${HELP}"
    exit ${EXIT_SUCCESS}
fi

## Test hosts
test_hosts="114.114.114.114"

## dependencies --- "sed, xxd, grep"
denpendencies=("sed" "xxd" "grep")
for dep in ${denpendencies[@]}; do
    if [ -z $(which ${dep}) ]; then
        echo "Need the ${dep} command"
        exit ${EXIT_FAIL}
    fi
done
echo "Denpendencies is ok!"

## Network
if !(ping -c 1 ${test_hosts} >> /dev/null); then
    echo "can't connect network!"
    exit ${EXIT_FAIL}
fi

## site check
if ( echo ${1} | grep -o "^\(\(http[:]\/\/\)\{0,1\}m\.wanben\.me\/xiaoshuo\/\)\{0,1\}[[:digit:]]*\/[[:digit:]]*\([/]\)\{0,1\}$" >> /dev/null ); then
    site_prefix=${1}
    if !( echo ${site_prefix} | grep -o "wanben" ); then
        site_prefix="m.wanben.me/xiaoshuo/"${site_prefix}
    fi
    if !(echo ${site_prefix} | grep -o "http" ); then
        site_prefix="http://"${site_prefix}
    fi
    if !(echo ${site_prefix} | grep -o "^.*[/]$" ); then
        site_prefix=${site_prefix}"/"
    fi
    echo -e "The site is \"${site_prefix}\"."
else
    echo -e "需要指定一个网址, 形式如下:\n\t\"[[http://]m.wanben.me/xiaoshuo/]<number>/<number>[/]\"\n"
    exit ${EXIT_FAIL}
fi

## contents check
rm -fr mulu.html
if ( wget -q ${site_prefix}"mulu.html" ); then
    contents_long=`cat mulu.html | grep -o "[[:space:]]\{1,\}[[:digit:]]\{1,5\}[^m]*<[/]ul>" | grep -o "[[:digit:]]*"`
    if [ -z ${contents_long} ]; then
        echo "unknow error in getting the contents long, exit"
        exit ${EXIT_FAIL}
    fi
else
    echo "Erroe, when processing to get contents"
    exit ${EXIT_FAIL}
fi
listnumber=`seq ${contents_long}`

## output_file
if [ ! -z ${2} ]; then
    output_file=${2}
else
    output_file=`grep -o "<h2>.*<[/]h2>" mulu.html | grep -o "[^<>h/]\{2,\}"`
    output_file=${output_file}".txt"
fi


# process in this function.
# parameter 1:<input file>
# parameter 2:<output file>
sub_the_html()
{
    if [ ! -f ${1} ]; then
        echo "can't access ${1}"
        return ${EXIT_FAIL};
    fi
    touch ${2}
    if [ -d /tmp ]; then
        tmp1=`mktemp /tmp/captrueXXXXXX`
        tmp2=`mktemp /tmp/captrueXXXXXX`
        tmp3=`mktemp /tmp/captrueXXXXXX`
        tmp4=`mktemp /tmp/captrueXXXXXX`
        tmp5=`mktemp /tmp/captrueXXXXXX`
    else
        tmp1=`mktemp`
        tmp2=`mktemp`
        tmp3=`mktemp`
        tmp4=`mktemp`
        tmp5=`mktemp`
    fi
    # 转化为Hexdump
    xxd -ps ${1} ${tmp1}
    # 再次转化, 为了去除换行符
    xxd -ps ${tmp1} ${tmp2}
    # 去除换行符
    sed -i "s/0a//g" ${tmp2}
    # 去除换行符后, 转换回去
    xxd -ps -r ${tmp2} ${tmp3}
    # 只需要<p>.*</p>部分
    grep -o "3c703e.*3c2f703e" ${tmp3} > ${tmp4}
    # 去除<br/>
    sed -i "s/0d3c62722f3e/0a20/g" ${tmp4}
    # 去除<p>
    sed -i "s/3c703e/20/g" ${tmp4}
    # 去除</p>
    sed -i "s/3c2f703e//g" ${tmp4}
    # 提取出来的文本
    xxd -ps -r ${tmp4} ${tmp5}
    # 去除HTML元素, 控制符号, 空白的行, 行中的空白
    sed -b -i "s/<\(.*\)>//g; s/\([<]*\|[>]*\)//g; s/[[:cntrl:]]//g; /^[[:space:]]*$/d; s/[[:space:]]//g" ${tmp5}
    # 章节前后留空
    sed -i "/[第].\{1,14\}[章]/a\ " ${tmp5}
    sed -i "/[第].\{1,14\}[章]/i\ " ${tmp5}
    sed -i "/[第].\{1,14\}[章]/i\ " ${tmp5}
    cat ${tmp5} >> ${2}

    ## delete the temp file
    rm -f ${tmp1} ${tmp2} ${tmp3} ${tmp4} ${tmp5}
    return ${EXIT_SUCCESS}
}

# Main part
## Main process
for numberlist in ${listnumber}; do
    echo "Begin processing ${numberlist} page;"
    if [ ! -f ${numberlist}".html" ]; then
        wget -q ${site_prefix}${numberlist}".html" >> /dev/null
    fi
    sub_the_html ${numberlist}".html" ${output_file}
    echo "Finish ${numberlist} page."
done

# Finish work
echo "OK!"
exit ${EXIT_SUCCESS}
