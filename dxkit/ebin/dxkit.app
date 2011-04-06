{application,
 dxkit,
 [{description,"Diagnostic and Monitoring Toolkit"},
  {vsn,"0.0.1"},
  {mod,{appstart_loader,[]}},
  {modules,[dxkit
           ,dxkit_net
           ,dxkit_sup
           ,dxkit_utils
           ,dxkit_world_server]},
  {registered,[]},
  {applications,[kernel
                ,stdlib
                ,riak_err
                ,fastlog]},
  {env,[
%    {world, [
%        {startup, {scan, all}}
%    ]},
    {appstart,[
        {startup, [dxkit_sup, start_link]}
    ]}
  ]}]}.