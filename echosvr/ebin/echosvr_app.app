{
	application,
	echosvr_app,
	[
		{description, "Echo server"},
		{vsn, "1.0"},
		{modules, [echosvr_app, echosvr_sup, echosvr]},
		{registered, echosvr},
		{applications, [kernel, stdlib]},
		{mod, {echosvr_app, []}},
		{env, [{timeout, 3000}]}
	]
}.


