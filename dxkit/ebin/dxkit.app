{application,
   dxkit,
   [{description,"Diagnostic and Monitoring Toolkit"},
    {vsn,"0.0.1"},
    {mod,{appstarter,[{fastlog, configure}]}},
    {modules,
        [dxkit,
         dxkit_event_bridge,
         dxkit_event_consumer,
         dxkit_event_handler_bridge,
         dxkit_event_sup,
         dxkit_event_log,
         dxkit_net,
         dxkit_net_sup,
         dxkit_subscription_sup,
         dxkit_sup,
         dxkit_sensor,
         dxkit_world]},
    {registered,[]},
    {applications,[kernel,
                   stdlib,
                   fastlog]},
    {env,[
      {appstart,[
          {startup, [dxkit_sup, start_link]}
      ]}
    ]}]}.
