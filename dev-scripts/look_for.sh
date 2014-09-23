#!/bin/bash
DIR_ROOT=$1
STRING=$2
find $DIR_ROOT -type f ! -path '*_build/*' -exec grep -nH "$STRING" {} \;
