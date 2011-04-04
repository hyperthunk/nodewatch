{application,dxweb,
 [{description,"Diagnostic and Monitoring Web Dashboard"},
  {vsn,"0.0.1"},
  {mod,{appstart_loader,[]}},
  {modules,[dxweb_sup, 
			dxweb_http_handler, 
			dxweb_http_server, 
			dxweb_util, 
			dxweb_websocket_registry,
			dxweb_session]},
  {registered,[]},
  {applications,[
    kernel,
	stdlib,
	%% required for unique session ids and used by misultin
    crypto,
	%% we use sasl for logging
    sasl,
    %% riak_err to keep the memory used during logging to a minimum
    %% TODO: integrate riak_err with fastlog to remove the burden from this app
    riak_err,
    %% fastlog for a consistent (and simple) logging api
    fastlog,
    %% we need to start dxkit in order to use various sensors
    dxkit,
    dxdb
  ]},
  {env,[
    {appstart,[
        {startup, [dxweb_sup, start_link]}
    ]}
  ]}]}.
