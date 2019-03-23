#!/bin/bash

SUBS_BAK=$HOME/mtd-backup
[ $# -eq 1 ] && ([ -d "$1" ] || ([ -d "${1%/*}" ] && mkdir "$1" || (echo "backup directory error.")))\
    || (echo "backup to $HOME/mtd-backup" && [ -d ${SUBS_BAK} ] || mkdir ${SUBS_BAK})
declare -r BAK_DIR=${1:-$HOME/mtd-backup}

[ -f /proc/mtd ] || (echo "MTD file don't exist." && exit 1)
declare -r MTD_TAB=$(tail -n +2 /proc/mtd)
declare -r MAX_REC=$(echo "${MTD_TAB}" | wc -l)

backup_mtd()
{
    [ $# -eq 2 ] || return 1
    ([ -c "$1" ] || [ -b "$1" ]) || return 1
    [ -f "$2" ] && mv -f $2 $2.bak
    dd if="$1" of="$2" 1>/dev/null 2>&1 && (echo "backup $2 success." && return 0) || return 1
}

cp -f /proc/mtd ${BAK_DIR} && echo "Backup /proc/mtd"
declare rec_loop
for((i=0;$i<${MAX_REC};i++)); do
    rec_loop=$(echo "${MTD_TAB}" | head -n $((i+1)) | tail -n 1)
    mtd_block="/dev/$(echo ${rec_loop} | cut -f 1 -d\ | cut -f 1 -d:)"
    mtd_name="$(echo ${rec_loop} | cut -f 4 -d\ | cut -f 2 -d\")"
    echo "backup $mtd_name"
    backup_mtd "$mtd_block" "$BAK_DIR/$mtd_name.mtd" || exit 1
done

exit 0
