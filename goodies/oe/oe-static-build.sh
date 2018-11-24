#! /bin/sh

# To be executed in the fu source directory

if csi -version | grep -q '^Version 5'; then
    chicken_version=5
else
    chicken_version=4
fi

# For CHICKEN 5:
[ "$chicken_version" = 5 ] && export CHICKEN_EGG_CACHE=$PWD

set -ex

if [ ! -e fu.scm ]; then
    echo "This doesn't seem to be fu's source directory." >&2
    exit 1
fi

if [ ! -e html-parser.o ]; then
    chicken-install -r html-parser >/dev/null
    csc -O3 -unit html-parser -emit-import-library html-parser -c html-parser/html-parser.scm -o html-parser.o
fi

if [ ! -e sxml-transforms.o ]; then
    chicken-install -r sxml-transforms >/dev/null
    cd sxml-transforms
    csc -O3 -unit sxml-transforms -emit-import-library sxml-transforms -c chicken/sxml-transforms.scm -o ../sxml-transforms.o
    cd ..
fi

cat <<EOF > builtin-goodies.scm
(include "goodies/oe/oe.scm")
(include "goodies/grep.scm")
EOF

if [ "$chicken_version" = 5 ]; then
    csc -A -m fu fu.scm
else
    csc -ASM -uses html-parser,sxml-transforms -c fu.scm
fi

csc -uses html-parser,sxml-transforms -c fu.scm -o fu.o
csc -static *o -o fu
strip fu
