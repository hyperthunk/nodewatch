#!/usr/bin/env sh

# set up the ERL_LIBS environment variable so that this project can be built from the top level
# as a cohesive whole - this script will push the current directory in front of your existing ERL_LIBS

case "$NODEWATCH_SETENV" in
    true)
        echo "./setenv.sh has already been run (ignoring)."
        ;;
    *)
        export PWD=`pwd`
        export ERL_LIBS="$PWD:$ERL_LIBS"
        export DXKIT_NET_CONF="$PWD/release/files"
        export NODEWATCH_SETENV="true"
        echo "Environment Setup Complete."
esac
