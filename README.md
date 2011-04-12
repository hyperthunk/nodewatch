# Nodewatch - A Diagnostic Monitoring Application for Erlang/OTP

Nodewatch is intended to assist with diagnostic (e.g. performance) monitoring
of Erlang/OTP software systems. It provides

1. A dynamic, web based _dashboard_ with streaming status/event data
2. A database in which to set up (per-user) configuration
3. A RESTful API for accessing configuration + status information
4. Tools for network/node discovery when using distributed erlang
5. Simple (file based) Node Monitoring Configuration (DB based in the future)
6. More stuff I haven't thought of yet (alarms, historical reports, etc)

### Why

Because sysadmins don't all love the shell as much as you think. Besides this,
sometimes you need to know what's up with your cluster when you're sitting way outside a corporate LAN/WAN, and connecting to a secure web application is a nice
clean way to get the visibility you need.

### How

Many awesome open source libraries have made _Nodewatch_ possible, not least
of which is [eper](https://github.com/massemanet/eper). _Nodewatch_ mainly
builds on existing capabilities.

### License

_Nodewatch_ is made available under a permissive, BSD-style open source license.
Please see the associated LICENSE file for details.

### Further information

See the [wiki](https://github.com/hyperthunk/nodewatch/wiki) for documentation
and FAQs. Other useful sources of information include

- The INSTALL file, for instructions on installing from binary and/or sources
- The NOTES file, which outlines the structure of the project sources
