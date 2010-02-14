{application, ../../pubbeer/pubbeer,
 [{description, "../../pubbeer/pubbeer"},
  {vsn, "0.01"},
  {modules, [
    ../../pubbeer/pubbeer,
    ../../pubbeer/pubbeer_app,
    ../../pubbeer/pubbeer_sup,
    ../../pubbeer/pubbeer_web,
    ../../pubbeer/pubbeer_deps
  ]},
  {registered, []},
  {mod, {../../pubbeer/pubbeer_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
