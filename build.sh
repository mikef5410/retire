#!/usr/bin/env bash

set -euo pipefail

LISP=$1
NAME=$(basename "$1" .lisp)
shift

/opt/sbcl/bin/sbcl --load "$LISP" \
     --eval "(sb-ext:save-lisp-and-die \"$NAME\"
               :executable t
               :compression 9
               :save-runtime-options t
               :toplevel '$NAME:toplevel)"
