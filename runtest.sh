#!/bin/bash

testsDir="$1"
test="$2"

thisScriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

echo $dir

shift
shift

runghc -i$thisScriptDir/src:$testsDir $test $@

