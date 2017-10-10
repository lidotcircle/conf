#!/usr/bin/env bash

HELP="
Usage:
    creat the boot image:
        androidboot.sh -c {--kernel <kernel>} {--ramdisk <ramdisk>} {--dt <dt>}
                          {--table <boot para file>}[--second <second-kernel>] -output <output file>
    decompress the boot image:
        androidboot.sh -d {--input <boot_image>} {--output <output_directory>}
    help:
        androidboot.sh {-h | --help}
"

if (echo $@ | grep '\(^-h$\|^--help$\)' >> /dev/null); then
    echo "$HELP"
    exit 0
fi

if [ -z $(which unpackbootimg) ] || [ -z $(which mkbootimg) ]; then
    echo "Error!"
    echo "Need 'unpackbootimg' and 'mkbootimg'."
    exit 1
fi

# parameter 1 : input file
# parameter 2 : output directory
unpackbootimg_func()
{
    # parameter check
    if [ ! ${#@} -eq 2 ]; then
        echo "Parameter count error in unpackbootimg_func() function"
        exit 1
    fi
    # the file check
    if [ ! -f $1 ]; then
        echo "The file \"$1\" don't exist."
        exit 1
    fi
    # the directory check
    if [ -e $2 ]; then
        echo "Remove the \"$2\" node."
        rm -rf $2
        mkdir $2
    else
        mkdir $2
    fi
    unpackbootimg -i ${1} -o ${2}
    # generate the boot parameter table
    cd ${2}
    boot_base=$(cat "${1}-base") && rm "${1}-base" && \
        echo "boot_base=${boot_base} " >> "${1}.par"
    boot_board=$(cat "${1}-board") && rm "${1}-board" && \
        echo "boot_board ${boot_board}" >> "${1}.par"
    boot_cmdline=$(cat "${1}-cmdline") && rm "${1}-cmdline" && \
        echo "boot_cmdline=\"${boot_cmdline}\"" >> "${1}.par"
    boot_hash=$(cat "${1}-hash") && rm "${1}-hash" && \
        echo "boot_hash=${boot_hash}" >> "${1}.par"
    boot_kerneloff=$(cat "${1}-kerneloff") && rm "${1}-kerneloff" && \
        echo "boot_kernel_offset=${boot_kerneloff}" >> "${1}.par"
    boot_oslevel=$(cat "${1}-oslevel") && rm "${1}-oslevel" && \
        echo "boot_os_patch_level=${boot_oslevel}" >> "${1}.par"
    boot_osversion=$(cat "${1}-osversion") && rm "${1}-osversion" && \
        echo "boot_os_version=${boot_osversion}" >> "${1}.par"
    boot_pagesize=$(cat "${1}-pagesize") && rm "${1}-pagesize" && \
        echo "boot_pagesize=${boot_pagesize}" >> "${1}.par"
    boot_ramdiskoff=$(cat "${1}-ramdiskoff") && rm "${1}-ramdiskoff" && \
        echo "boot_ramdisk_offset=${boot_ramdiskoff}" >> "${1}.par"
    boot_secondoff=$(cat "${1}-secondoff") && rm "${1}-secondoff" && \
        echo "boot_second_offset=${boot_secondoff}" >> "${1}.par"
    boot_tagoff=$(cat "${1}-tagsoff") && rm "${1}-tagsoff" && \
        echo "boot_tags_offset=${boot_tagoff}" >> "${1}.par"
    return 0
}

# parameter 1 : kernel
# parameter 2 : ramdisk
# parameter 3 : dt
# parameter 4 : output file
# parameter 5 : parameter table file
# parameter 6 : second ( option )
packbootimg()
{
    # parameter check
    if [ ${#@} -lt 5 ]; then
        echo "Parameter count error in packbootimg() function"
        exit 1
    elif [ ${#@} -eq 5 ]; then
        option=""
    else
        option="--second ${6}"
    fi
    i=1; temp_eval=`sed -n "${i}p" ${5}`
    while [ ! -z "${temp_eval}" ]; do
        if (echo ${temp_eval} | grep '[=]' >> /dev/null); then
            eval ${temp_eval}
        fi
        i=$[${i} + 1]
        temp_eval=`sed -n "${i}p" ${5}`
    done
    mkbootimg --kernel ${1} --ramdisk ${2} --dt ${3}\
        --base ${boot_base} --pagesize ${boot_pagesize}\
        --cmdline "${boot_cmdline}"\
        --kernel_offset ${boot_kernel_offset} --ramdisk_offset ${boot_ramdisk_offset}\
        --second_offset ${boot_second_offset} --tags_offset ${boot_tags_offset}\
        --os_version ${boot_os_version} --os_patch_level ${boot_os_patch_level}\
        --hash ${boot_hash} ${option} -o ${4}
    return 0
}

flags=0
mode=-1
# deal with parameters
while [ ! -z $1 ]; do
    if [ $1 = "-c" ]; then
        if [ $flags -eq 0 ]; then
            mode=0
            flags=1
        else
            echo "choose first method to continue."
        fi
        shift && continue
    elif [ $1 = "-d" ]; then
        if [ $flags -eq 0 ]; then
            mode=1
            flags=1
        else
            echo "choose first method to continue."
        fi
        shift && continue
    elif [ $1 = "--kernel" ]; then
        KERNEL=$2
        shift 2 && continue
    elif [ $1 = "--second" ]; then
        SECOND=$2
        shift 2 && continue
    elif [ $1 = "--ramdisk" ]; then
        RAMDISK=$2
        shift 2 && continue
    elif [ $1 = "--dt" ]; then
        DT=$2
        shift 2 && continue
    elif [ $1 = "--table" ]; then
        TABLE=$2
        shift 2 && continue
    elif [ $1 = "--input" ]; then
        INPUT=$2
        shift 2 && continue
    elif [ $1 = "--output" ]; then
        OUTPUT=$2
        shift 2 && continue
    else
        echo "Error parameter \"$1\"."
        shift
    fi
done

# parameter recheck
if [ $mode -eq 0 ]; then
    if [ -z $KERNEL ] || [ -z $RAMDISK ] || [ -z $DT ] || [ -z $OUTPUT ] || [ -z $TABLE ]; then
        echo "To creat a boot image need \"kernel ramdisk dt table\" specify."
        echo "$HELP"
        echo "Exit!"
        exit 1
    fi
    if [ ! -z $INPUT ]; then
        echo "can't specify \"--input\" parameter in creat boot image."
        echo "$HELP"
        echo "Exit!"
        exit 1
    fi
elif [ $mode -eq 1 ]; then
    if [ -z $OUTPUT ] || [ -z $INPUT ]; then
        echo "To decompress a boot image need \"input file and output file\" specify."
        echo "$HELP"
        echo "Exit!"
        exit 1
    fi
    if [ ! -z $KERNEL$RAMDISK$DT$SECOND ]; then
        echo "can't spcify \"--kernel\", \"--ramdisk\", \"--dt\" and \"--second\" parameter in decompress the boot image."
        echo "$HELP"
        echo "Exit!"
        exit 1
    fi
else
    echo "Error specify the mode, \"-c\" or \"-d\"."
    echo "$HELP"
    echo "Exit!"
    exit 1
fi

while (true); do
    break
    echo "OUTPUT is $OUTPUT"
    echo "INPUT is $INPUT"
    exit 0
done

if [ $mode -eq 1 ]; then
    unpackbootimg_func $INPUT $OUTPUT
else
    if [ -z $SECOND ]; then
        packbootimg $KERNEL $RAMDISK $DT $OUTPUT $TABLE
    else
        packbootimg $KERNEL $RAMDISK $DT $OUTPUT $TABLE $SECOND
    fi
fi
