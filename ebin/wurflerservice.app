{application, wurflerservice,
 [
  {description, "A Wurfl file service"},
  {vsn, "0.1.0"},
  {modules, [
  			 wurfler_config,
             wurfler,
             wurfler_cache,
             wurflerservice,
             wurfler_importer,
             wurfler_update,
             wurfler_deps,
             xml_factory,
             static_resource,
             device_resource,
             devices_resource,
	     brand_resource,
	     brands_resource,
             wurfler_db,             
             wurfler_file_poller,
	wurfler_patch,
		wurfler_date_util,
	% ErlyDTL templates
			about_dtl,
			device_dtl,
			devices_dtl,
			brands_dtl,
			brand_dtl
            ]},
  {registered, [wurflerservice]},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { wurflerservice, []}},
  {env, []}
 ]}.
