#! /bin/sh

# To be executed in the fu source directory

if csi -version | grep -q '^Version 5'; then
    chicken_version=5
else
    chicken_version=4
fi

set -ex

if [ ! -e fu.scm ]; then
    echo "This doesn't seem to be fu's source directory." >&2
    exit 1
fi

cat <<EOF > builtin-goodies.scm
(include "goodies/oe/oe.scm")
(include "goodies/grep.scm")
EOF

if [ "$chicken_version" = 5 ]; then
    csc -A -m fu fu.scm
    LIBDIR=$(csi -p '(begin (import (chicken platform)) (car (repository-path)))')
    csc -L -static -static -o fu "$LIBDIR/srfi-1.o" "$LIBDIR/srfi-13.o" fu.scm -o fu
else
    csc -ASM -c fu.scm
    csc -c fu.scm -o fu.o
    csc -static *o -o fu
fi

strip fu
