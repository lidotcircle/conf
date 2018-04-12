#!/usr/bin/env bash

HELP="
Usage:
        ramdisk.sh {-d -i <ramdisk-file> -o <ramdisk_directory> | -c -i <ramdisk_directory> -o <ramdisk_file>}

Option:
        -d      decompress the ramdisk file
        -c      compress the ramdisk file
        -i      input file or directory
        -o      output file or directory

Description:
        unpack and pack the android ramdisk.
"

if [ ${1} = "-h" ] || [ ${1} = "--help" ]; then
    echo "$HELP"
    exit 0
fi

if (echo $@ | grep "[-][cd]" >> /dev/null) && (echo $@ | grep "[-]i" >> /dev/null) && (echo $@ | grep "[-]o" >> /dev/null); then
    echo "Begin!"
else
    echo "Error parameter!"
    echo "${HELP}"
    exit 1
fi

flags=0

while [ ! -z $1 ]; do
    if [ $1 = "-d" ]; then
        if [ ${flags} -eq 0 ]; then
            mode=1
            flags=1
            shift
            continue
        else
            echo "Error parameter \"${1}\"!"
            exit 1
        fi
    elif [ $1 = "-c" ]; then
        if [ ${flags} -eq 0 ]; then
            mode=0
            flags=1
            shift
            continue
        else
            echo "Error parameter \"${1}\"!"
            exit 1
        fi
    elif [ $1 = "-i" ]; then
        if [ ! -z ${2} ]; then
            INPUT=${2}
        else
            echo "Error parameter after \"$1\""
            exit 1
        fi
        shift 2
        continue
    elif [ $1 = "-o" ]; then
        if [ ! -z ${2} ]; then
            OUTPUT=${2}
        else
            echo "Error parameter after \"$1\""
            exit 1
        fi
        shift 2
        continue
    else
        echo "Error parameter \"${1}\""
        shift
    fi
done

decompreess_cpio()
{
    if [ ! ${#@} -eq 2 ]; then
        echo "parameter error in function decompreess_cpio()."
        return 1
    fi
    if [ ! -f ${1} ]; then
        echo "The file \"${1}\" don't exist."
        return 1
    fi
    if [ -e ${2} ]; then
        echo "Remove the \"${2}\" directory..."
        rm -rf ${2}
        mkdir ${2}
    else
        mkdir ${2}
    fi
    cp ${1} ${2}
    cd ${2}
    mv ${1} "${1}.gz"
    gzip -d "${1}.gz"
    cpio -i < ${1}
    rm ${1}
    cd ../
    return 0
}

compress_cpio()
{
    if [ ! ${#@} -eq 2 ]; then
        echo "parameter error in function compress_cpio()."
        return 1
    fi
    if [ ! -d ${2} ]; then
        echo "The directory \"${2}\" don't exist."
        return 1
    fi
    cd ${2}
    find -regex '.[^.].*' | cpio -H newc -o > ../${1}
    cd ../
    gzip ${1}
    return 0
}

while (true); do
    if [ $mode -eq 1 ]; then
        decompreess_cpio ${INPUT} ${OUTPUT}
    else
        compress_cpio ${OUTPUT} ${INPUT}
    fi
    break
done

exit 0
