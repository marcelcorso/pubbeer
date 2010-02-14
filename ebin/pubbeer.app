{application, pubbeer,
 [{description, "pubbeer"},
  {vsn, "0.01"},
  {modules, [
    pubbeer,
    pubbeer_app,
    pubbeer_sup,
    pubbeer_web,
    pubbeer_deps
  ]},
  {registered, []},
  {mod, {pubbeer_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
