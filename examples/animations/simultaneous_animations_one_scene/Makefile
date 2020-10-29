# This Makefile uses castle-engine build tool for most operations.
# See https://github.com/castle-engine/castle-engine/wiki/Build-Tool .

MODE:=debug

.PHONY: compile
compile:
	castle-engine --mode=$(MODE) compile $(CASTLE_ENGINE_TOOL_OPTIONS)

.PHONY: clean
clean:
	castle-engine clean

.PHONY: win32
win32:
	castle-engine --mode=$(MODE) package --os=win32 --cpu=i386

.PHONY: win64
win64:
	castle-engine --mode=$(MODE) package --os=win64 --cpu=x86_64

.PHONY: linux32
linux32:
	castle-engine --mode=$(MODE) package --os=linux --cpu=i386

.PHONY: linux64
linux64:
	castle-engine --mode=$(MODE) package --os=linux --cpu=x86_64

.PHONY: src
src:
	castle-engine --mode=$(MODE) package-source

.PHONY: android
android:
	castle-engine --mode=$(MODE) package --target=android --fast
	castle-engine install --target=android
	castle-engine run --target=android

.PHONY: ios
ios:
	castle-engine --mode=$(MODE) package --target=iOS --fast
