{application, wurfler,
 [
  {description, "A Wurfl file service"},
  {vsn, "1"},
  {modules, [
             wurfler,
             wurfler_service,
             wurfler_deps
            ]},
  {registered, [wurfler_service]},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { wurfler_service, []}},
  {env, []}
 ]}.
