{application, wurflerservice,
 [
  {description, "A Wurfl file service"},
  {vsn, "0.1"},
  {modules, [
  			 wurfler_config,
             wurfler,
             wurflerservice,
             wurfler_importer,
             wurfler_deps,
             xml_factory,
             device_resource,
             devices_resource,
             wurfler_db,             
             wurfler_file_poller,
	 wurfler_patch,
	 wurfler_SUITE
	
            ]},
  {registered, [wurflerservice]},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { wurflerservice, []}},
  {env, []}
 ]}.
