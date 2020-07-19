Demonstrates `ApplicationProperties.OnLog`, which allows you to register
custom callbacks that receive all log messages.
You can use this e.g. to send logs remotely.

Demo:

- Just run and press any keys or mouse buttons.
  They will cause `WritelnLog`, which will be send to your server usign asynchronours HTTP POST.

- Be sure to edit your server name in `gameloghandler.pas`.
  You need a simplest HTTP server that handles PHP.
  Use the PHP code from `sample_php_server_logger/cge_logger.php`.

Note that only logs send to CGE CastleLog (like WritelnLog, WritelnWarning)
are captured this way.
Other stuff being written e.g. to stdout or stderr is not captured
using `ApplicationProperties.OnLog`.
