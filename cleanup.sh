#!/bin/sh
DIRS="src/FsTables"
for dir in $DIRS; do
	rm -rf $dir/bin $dir/obj
done
