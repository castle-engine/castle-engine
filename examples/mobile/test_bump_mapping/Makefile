# This Makefile uses castle-engine build tool for most operations.
# See https://github.com/castle-engine/castle-engine/wiki/Build-Tool .

.PHONY: standalone
standalone:
	castle-engine compile --mode=debug $(CASTLE_ENGINE_TOOL_OPTIONS)

.PHONY: clean
clean:
	castle-engine clean

.PHONY: android
android:
	castle-engine package --os=android --cpu=arm --fast --mode=debug
	castle-engine install --os=android --cpu=arm
	castle-engine run --os=android --cpu=arm
