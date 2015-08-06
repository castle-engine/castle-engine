# Helper script to run Firefox with some debug options.
# Adjust as necessary for your system!

# Note that you can disable dom.ipc.plugins.enabled
# (see http://www.firebreath.org/display/documentation/Debugging+Plugins docs)
# in about:config for "single process", makes it much easier to see plugin outout
# (stdout, stderr) directly in Firefox.
#
# Although that's not so important on Windows, where our WritelnLog goes to
# a special xxx.log file anyway (in user config directory, xxx.log where
# xxx is your ApplicationName).

export MOZ_CRASHREPORTER_DISABLE=1
"c:/Program Files/Mozilla Firefox/firefox.exe"  --console --jsconsole  --no-remote -P test
