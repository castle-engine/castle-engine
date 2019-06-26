# This Makefile uses castle-engine build tool for most operations.
# See https://github.com/castle-engine/castle-engine/wiki/Build-Tool .

.PHONY: standalone
standalone:
	castle-engine compile $(CASTLE_ENGINE_TOOL_OPTIONS)

.PHONY: clean
clean:
	castle-engine clean

.PHONY: release-src
release-src:
	castle-engine package-source

.PHONY: release-win32
release-win32:
	castle-engine package --os=win32 --cpu=i386

.PHONY: release-linux
release-linux:
	castle-engine package --os=linux --cpu=i386

.PHONY: release-linux64
release-linux64:
	castle-engine package --os=linux --cpu=x86_64

.PHONY: android
android:
	castle-engine package --target=android --fast --mode=debug
	castle-engine install --target=android
	castle-engine run --target=android

.PHONY: release-android
release-android:
	castle-engine package --target=android --mode=release
	castle-engine install --target=android
