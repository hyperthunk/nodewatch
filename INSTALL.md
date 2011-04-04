# Installation

This document provides instructions on installing *nodewatch* either from a 
release tarball or from sources. It is highly recommended that you install 
a binary release rather than building from sources, as the process is far 
less complicated.

## Installing from release tarball

TBC

## Installing from sources

You will need to fetch any missing dependencies in order to build from sources. 
You can do this by running the `install_deps` script in the top directory. 

### Getting Dependencies 

You may also check for missing dependencies by running `rebar check-deps`, 
and if you've already installed the dependencies into your environment then they
only need to be on the code server's path (e.g. somewhere under your `ERL_LIBS`).

In order to generate a release, there is an additional dependency on the erlydtl
templating library. This will be obtained automatically by `install_deps` if it
is missing. 

### Compiling

