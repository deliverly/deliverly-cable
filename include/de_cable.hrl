-record(de_cable_message, {
	command :: subscribe | unsubscribe | message,
	channel = <<"">> :: binary(),
	data = <<"">> :: binary()
}).