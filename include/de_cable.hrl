-record(de_cable_message, {
	command :: subscribe | unsubscribe | message,
	channel = <<"">> :: binary(),
	data = <<"">> :: binary()
}).

-define(Config(X,Y), maps:get(X, ulitos_app:get_var(deliverly, cable, #{}), Y)).
