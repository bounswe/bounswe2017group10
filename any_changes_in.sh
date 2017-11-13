#!/bin/bash


project=$1
num_changed_files=`git diff --name-only master | grep -E "^Atlas/${project}" | wc -l`

[[ ${num_changed_files} -ne 0 ]]
exit $?
