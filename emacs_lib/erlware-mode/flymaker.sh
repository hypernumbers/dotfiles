#!/bin/bash

echo $1

Is=""
PAs=""
for i in `dirname $1`/../../*/include; do
    Is="-I $i $Is"
done
for e in `dirname $1`/../../*/ebin; do
    PAs="-pa $e $PAs"
done

erlc -o /tmp $Is $PAs +no_error_module_mismatch -Wall $1
