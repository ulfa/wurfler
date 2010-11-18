{application, wurflerservice,
 [
  {description, "A Wurfl file service"},
  {vsn, "0.1"},
  {modules, [
             wurfler,
             wurflerservice,
             wurfler_deps,
             device_resource
            ]},
  {registered, [wurflerservice, wurfler]},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { wurflerservice, []}},
  {env, []}
 ]}.
