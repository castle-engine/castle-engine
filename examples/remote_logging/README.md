Demonstrates `ApplicationProperties.OnLog`, which allows you to register
custom callbacks that receive all log messages.
You can use this e.g. to send logs remotely.

Note that only logs send to CGE CastleLog (like WritelnLog, WritelnWarning)
are captured this way.
Other stuff being written e.g. to stdout or stderr are not captured.
