{application,dxdb,
 [{description,"System Monitoring Database"},
  {vsn,"0.0.1"},
  {modules,[dxdb_schema, dxdb_sup, dxdb]},
  {registered,[dxdb_schema]},
  {applications,[
    kernel,
	stdlib,
    crypto,
    sasl,
    riak_err,
    fastlog
	%% NB: we DO NOT start mnesia using appstart - 
	%%  rather we do this manually in dxdb:prepare_start/0
  ]},
  {env,[]}]}.
