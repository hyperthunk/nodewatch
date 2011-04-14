#!/usr/bin/env python

# helper for messing around with test nodes - runs a command like
# escript release/files/nodetool -name <name>@<host> <command>
# commands tend to be 'ping', 'stop' and 'restart' (calls to init)
# whilst passing the command start will start up a new node.

import subprocess
from subprocess import check_output, CalledProcessError

def nodetool(name, cmd):
    if cmd == 'start':
        args = ['erl', '-noshell', '-detached',
                '-name', name]
    else:
        args = ["escript", "release/files/nodetool",
                "-name", name, cmd]
    try:
        sout = check_output(args, stderr=subprocess.STDOUT)
        print(sout)
    except CalledProcessError, e:
        print(e.output)

if __name__ == '__main__':
    import sys
    # print("%s" % sys.argv[1:])
    nodetool(*(sys.argv[1:]))
