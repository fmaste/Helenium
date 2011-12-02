#!/bin/bash

cd src

test="$1"

shift

runghc Test.$test $@

