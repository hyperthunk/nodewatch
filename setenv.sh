#!/usr/bin/env sh

# set up the ERL_LIBS environment variable so that this project can be built from the top level
# as a cohesive whole - this script will push the current directory in front of your existing ERL_LIBS

export PWD=`pwd`
export DEPS_DIR=`cat rebar.config | grep deps_dir | awk ' /.*/ { gsub("\\\[\"", ""); gsub("\"\\]\\}.", ""); print $2; }'`
#export ERL_LIBS="$PWD:$PWD/$DEPS_DIR:$ERL_LIBS"
export ERL_LIBS="$PWD:$ERL_LIBS"
