{application, 'gen_srv', [
	{description, "New project"},
	{vsn, "0.1.0"},
	{modules, ['gen_srv','gen_srv_app','gen_srv_sup']},
	{registered, [gen_srv_sup]},
	{applications, [kernel,stdlib]},
	{mod, {gen_srv_app, []}},
	{env, []}
]}.