#!/usr/bin/env sh

for var in "$@"
do
    case "$var" in
        -h)
            echo "Usage: ./devmode.sh -t 5.0 -s 2"
            exit 0
    esac
done

if [ ! -d "$DEVENV" ]; then
    echo "You need to install https://github.com/n0gg1n/devenv to use this"
else
    ruby "$DEVENV/devenv.rb" -c 'cache.yml' $@ 'rebar compile'
fi
