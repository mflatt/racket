#!/bin/sh

MACH=$1
BOOSTRAPPED_DIR=$2
SCHEME_DIR=$3

check_and_copy_pb()
{
    SRC=$BOOSTRAPPED_DIR/pb/$1
    DEST=$SCHEME_DIR/boot/pb/$1
    if [ ! -e $DEST ] ; then
        cp $SRC $DEST
        touch boot_pending
    elif [ $SRC -nt $DEST ] ; then
        cp $SRC $DEST
        touch boot_pending
    fi
}

mkdir -p $SCHEME_DIR/boot/pb
check_and_copy_pb scheme.boot
check_and_copy_pb petite.boot
check_and_copy_pb scheme.h
check_and_copy_pb equates.h
check_and_copy_pb gc-ocd.inc
check_and_copy_pb gc-oce.inc
check_and_copy_pb vfasl.inc

check_mach()
{
    INIT=$SCHEME_DIR/boot/$MACH/$1
    if [ ! -e $INIT ] ; then
        touch boot_pending
    fi
}

check_mach scheme.boot
check_mach petite.boot
check_mach scheme.h
check_mach equates.h
check_mach gc-ocd.inc
check_mach gc-oce.inc
check_mach vfasl.inc
