# This Makefile uses castle-engine build tool for most operations.
# See https://sourceforge.net/p/castle-engine/wiki/Build%20tool/ .

.PHONY: standalone
standalone:
	castle-engine compile $(CASTLE_ENGINE_TOOL_OPTIONS)

.PHONY: clean
clean:
	castle-engine clean
	$(MAKE) -C android/ clean

.PHONY: release-win32
release-win32:
	castle-engine package --os=win32 --cpu=i386

.PHONY: release-linux
release-linux:
	castle-engine package --os=linux --cpu=i386

.PHONY: release-src
release-src:
	castle-engine package-source
