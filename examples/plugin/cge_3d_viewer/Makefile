# This Makefile uses castle-engine build tool for most operations.
# See https://github.com/castle-engine/castle-engine/wiki/Build-Tool .

.PHONY: plugin
plugin:
# 64-bit plugin dll seems rather useless,
# at least on Windows browsers don't use it anyway.
#	castle-engine compile --os=win64 --cpu=x86_64 --plugin $(CASTLE_ENGINE_TOOL_OPTIONS)
	castle-engine compile --plugin $(CASTLE_ENGINE_TOOL_OPTIONS)
	castle-engine install --plugin

.PHONY: standalone
standalone:
	castle-engine compile --mode=debug $(CASTLE_ENGINE_TOOL_OPTIONS)

.PHONY: clean
clean:
	castle-engine clean

# .PHONY: release-win32
# release-win32:
# 	castle-engine package --os=win32 --cpu=i386

# .PHONY: release-linux
# release-linux:
# 	castle-engine package --os=linux --cpu=i386

# .PHONY: release-src
# release-src:
# 	castle-engine package-source

# .PHONY: release-android
# release-android:
# 	castle-engine package --target=android
# 	castle-engine install --target=android
