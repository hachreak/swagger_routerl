#!/bin/sh

DIR=`dirname $0`

# start tests
cd $DIR
./utils/rebar3 do compile, eunit, cover -v
