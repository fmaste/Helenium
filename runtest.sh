#!/bin/bash

testsDir="$1"
test="$2"

shift
shift

runghc -isrc:$testsDir $test $@

