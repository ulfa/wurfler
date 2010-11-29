{application, wurflerservice,
 [
  {description, "A Wurfl file service"},
  {vsn, "0.1"},
  {modules, [
             wurfler,
             wurflerservice,
             wurfler_deps,
             xml_factory,
             device_resource,
             devices_resource,
             wurfler_db
            ]},
  {registered, [wurflerservice]},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { wurflerservice, []}},
  {env, []}
 ]}.
