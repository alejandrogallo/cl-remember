#!/usr/bin/env bash
CONFIG_FILES=(
linode.lisp
init.lisp
)

select f in ${CONFIG_FILES[@]}; do
	sbcl --load $f
done
