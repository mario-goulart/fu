#! /bin/sh

# To be executed in the fu source directory

set -ex

chicken-install -r html-parser >/dev/null
chicken-install -r sxml-transforms >/dev/null

csc -unit html-parser -emit-import-library html-parser -c html-parser/html-parser.scm -o html-parser.o

cd sxml-transforms
csc -unit sxml-transforms -emit-import-library sxml-transforms -c chicken/sxml-transforms.scm -o ../sxml-transforms.o
cd ..

cat <<EOF > builtin-goodies.scm
(include "$HOME/src/git/configs/goodies/oe/oe.scm")
(include "$HOME/src/github/fu/goodies/grep.scm")
EOF

csc -uses html-parser,sxml-transforms -c fu.scm -o fu.o
csc -static *o -o fu
strip fu
