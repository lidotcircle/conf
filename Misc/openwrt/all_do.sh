#!/bin/bash

# log
declare -r INF_LOG="/var/log/all_do_out.log"
declare -r ERR_LOG="/var/log/all_do_err.log"

#{ USAGE()
USAGE()
{
    cat <<EOF
all_do.sh [options]
    
    options:
      -s      suppress standard output (default)
      -p      invert -s option
      -h      print help
EOF
} #}

PRINT_OUT=0
[ $# -gt 1 ] && USAGE && exit 1
if [ $# -eq 1 ]; then
    if [ "$1" == "-p" ]; then
        PRINT_OUT=1
    elif [ "$1" == "-h" ]; then
        USAGE && exit 0
    else
        if [ ! "$1" == "-s" ]; then
            USAGE && exit 1
        fi
    fi
fi

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
        if [ $PRINT_OUT -eq 1 ]; then
            echo -e "$MSG" | tee ${INF_LOG}
        else
            echo -e "$MSG" >> ${INF_LOG}
        fi
        return 0
    fi
    if [ "${PRIORITY}" == "ERROR" ]; then
        echo -e "ERROR [$(date)]: $MSG" | tee ${ERR_LOG}
        return 0
    else
        if [ $PRINT_OUT -eq 1 ]; then
            echo -e "${PRIORITY} [$(date)]: $MSG" | tee ${INF_LOG}
        else
            echo -e "${PRIORITY} [$(date)]: $MSG" >> ${INF_LOG}
        fi
        return 0
    fi
} #}

 #{ function: clean_exit(exit_status)
clean_exit()
{
    __logger__ "function clean_exit() is called."
    [ "${START_BANNER}" == 1 ] && __logger__ "BANNER" "----------------- END $$ ------------------\n"
    [ $# -eq 1 ] && exit $1
    exit 1
} #}


opkg update
packages_list=$(opkg list | cut -d\  -f 1)
packages_list_installed=$(opkg list-installed | cut -d\  -f 1)
#{ function: in_packages() => 0: true, 1 false
in_packages()
{
    local test_str=$(echo ${packages_list} | grep "^$1$")
    if [ "$test_str" == "$1" ] && [ "$1" != "" ]; then
        return 0
    fi
    __logger__ "DEBUG" "Test package <$1> fail"
    return 1
} #}
#{ function: has_installed() => 0: true, 1 false
has_installed()
{
    local test_str=$(echo ${packages_list_installed} | grep "^$1$")
    if [ "$test_str" == "$1" ] && [ "$1" != "" ]; then
        return 0
    fi
    return 1
} #}

#{ function: opkg_install(... packages)
opkg_install()
{
    __logger__ "opkg_install() is called."
    __logger__ "install [$@]"
    local IFS=$' '
    local install_list=
    for package in $@; do
        if !(has_installed $package); then
            if (in_packages $package); then
                install_list="$install_list $package"
                continue
            else
                __logger__ "ERROR" "unknown package ${package}, exit 1"
                clean_exit 1
            fi
        fi
        echo -e "package ${package} has already installed."
    done
    if [ -n "$install_list" ]; then
        opkg install $install_list
        if [ ! $? -eq 0 ]; then
            __logger__ "ERROR" "opkg install fail"
            clean_exit 1
        fi
    fi
    return 0
} #}

__logger__ "BANNER" "\n---------------- BEGIN $$ -----------------"

# usb and filesystem
opkg_install e2fsprogs kmod-usb-storage kmod-usb2 kmod-usb3
opkg_install ntfs-3g ntfs-3g-utils kmod-fs-ext4

if [ ! "$(ls -al /dev/sd* | wc -l)" == "0" ]; then
    block detect | sed "s/\/mnt\/sd.\([[:digit:]]\)/\/root\/usb\1/g" | \
        sed "/option/s/enabled\([[:blank:]]*\)'0'/enabled '1'/g; /option/s/check_fs\([[:blank:]]*\)'0'/check_fs\1'1'/g" | uci import fstab
    devs=`block detect | sed -n "/\/mnt\/sd/p" | grep -oe "sd[a-z][[:digit:]]"`
    # uci set fstab.@mount[0].enabled='1' && uci set fstab.@global[0].check_fs='1' && uci commit
    for usb_mount_dir in $(cat /etc/config/fstab | sed -n '/target/p' | grep -oe "[^']*" | cut -d\  -f 3); do
        __logger__ "INFO" "a partition will mount at $usb_mount_dir"
        if [ ! -e ${usb_mount_dir} ]; then
            __logger__ "mkdir -p ${usb_mount_dir}"
            mkdir -p ${usb_mount_dir}
        elif [ ! -d ${usb_mount_dir} ]; then
            rm -f ${usb_mount_dir}
            __logger__ "mkdir -p ${usb_mount_dir}"
            mkdir -p ${usb_mount_dir}
        fi
    done
    [ -x /sbin/block ] && [ -x /etc/init.d/fstab ] && \
        /sbin/block mount &&  /etc/init.d/fstab enable
    mount_info=$(mount)
    for dev__ in $devs; do
        if [ -n "$(echo ${mount_info} | grep -oe ${dev__})" ]; then
            continue
        fi
        mount_point=$(echo ${dev__} | sed "s/.*\([[:digit:]]\)/\/root\/usb\1/g")
        __dev__=$(echo ${dev__} | sed "s/^/\/dev\//g")
        __logger__ "INFO" "mount: [$__dev__ to $mount_point]"
        mount $__dev__ $mount_point
    done
fi

opkg_install git git-http
opkg_install vim-fuller

opkg_install curl netcat tcpdump ss nmap
opkg_install tar file lsof

# shadowsocksr
opkg_install luci-app-ssr-pro luci-i18n-ssr-pro-zh-cn

clean_exit 0
