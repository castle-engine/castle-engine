# This Makefile uses castle-engine build tool for most operations.
# See https://github.com/castle-engine/castle-engine/wiki/Build-Tool .

MODE:=debug

.PHONY: standalone
standalone:
	castle-engine --mode=$(MODE) compile $(CASTLE_ENGINE_TOOL_OPTIONS)

.PHONY: clean
clean:
	castle-engine clean

.PHONY: android
android:
	castle-engine --mode=$(MODE) package --os=android --cpu=arm
	castle-engine install --os=android --cpu=arm
	castle-engine run --os=android --cpu=arm

.PHONY: ios
ios:
	castle-engine --mode=$(MODE) package --target=iOS --fast
