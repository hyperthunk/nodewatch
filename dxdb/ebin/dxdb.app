{application,dxdb,
 [{description,"System Monitoring Database"},
  {vsn,"0.0.1"},
  {mod,{appstart_loader,[]}},
  {modules,[dxdb_app, dxdb_schema, dxdb_sup, dxdb]},
  {registered,[dxdb_schema]},
  {applications,[
    kernel,
	stdlib,
    crypto,
    sasl,
    riak_err,
    fastlog
	%% NB: we DO NOT start mnesia using appstart
  ]},
  {env,[
    {appstart,[
        {startup, [dxdb_sup, full_start]}
    ]}
  ]}]}.
