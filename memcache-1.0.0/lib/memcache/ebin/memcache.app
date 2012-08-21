{
	application,
	memcache,
	[
		{description, "memory cache server"},
		{vsn, "1.0.0"},
		{modules, [mc_app, mc_sup, mc_server]},
		{registered, [mc_server]},
		{applications, [kernel, stdlib]},
		{mod, {mc_app, []}},
		{env, [{timeout, 3000}]}
	]
}.


