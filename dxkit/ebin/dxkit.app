{application,
 dxkit,
 [{description,"Diagnostic and Monitoring Toolkit"},
  {vsn,"0.0.1"},
  {mod,{appstart_loader,[]}},
  {modules,[dxkit_net,dxkit_sup,dxkit_utils,dxkit_world_server]},
  {registered,[]},
  {applications,[kernel,stdlib]},
  {env,[
    {appstart,[
        {startup, [dxkit_sup, start_link]}
    ]}
  ]}]}.