# Installation

This document provides instructions on installing *nodewatch* either from a
release tarball or from sources. It is highly recommended that you install
a binary release rather than building from sources, as the process is far
less complicated.

## Installing from release tarball

TBC

## Installing from sources

You will need to fetch any missing dependencies in order to build from sources.
You can do this by running the `bootstrap` script in the top directory.
Running this script will install *all* dependencies locally (into the `lib`
directory) and compile against them, without affecting your `code:lib_dir`
and/or `ERL_LIBS` directories.

### Compiling

Providing [rebar](http://github.com/basho/rebar) can resolve all the required
dependencies, you may compile all the project's sources with the shell command:

	rebar compile skip_deps=true

If you need to acquire and/or compile dependencies, you're advised to run the
`bootstrap` script instead. The release generation process relies on the 
current `git tag` to figure out the current release version. 

### Getting Dependencies

You may also check for missing dependencies by running `rebar check-deps`,
and if you've already installed the dependencies into your environment then they
only need to be on the code server's path (e.g. somewhere under your `ERL_LIBS`).

In order to generate a release, there is an additional dependency on the erlydtl
templating library. This will be obtained automatically by `bootstrap` if it
is missing.
