#!/usr/bin/env bash

set -eu

: ${CC=clang}
: ${CFLAGS=}
: ${LDFLAGS=}

TARGET="assemblar8080"
CFLAGS="$CFLAGS -std=c99"
LDFLAGS="$LDFLAGS -lm"

panic() {
    printf "%s\n" "$1"
    exit 1
}

build_assemblar() {
    case $1 in
    debug)
        EXTRAFLAGS="-Wall -Wextra -pedantic -ggdb -DDebug -fsanitize=address"
        ;;

    release)
        EXTRAFLAGS="-O3 -DRelease"
        ;;

    *)
        panic "Build mode unsupported!"
    esac

    set -x
    $CC $CFLAGS $EXTRAFLAGS $LDFLAGS assemblar.c -o $TARGET
    set +x
}

if [[ $# -eq 0 ]]; then
    build_assemblar debug
    exit 0
fi

if [[ $# -eq 1 ]]; then
    build_assemblar $1
    exit 0
else
    panic "Too many arguments"
fi
