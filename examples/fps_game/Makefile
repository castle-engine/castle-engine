# This Makefile uses castle-engine build tool for most operations.
# See https://github.com/castle-engine/castle-engine/wiki/Build-Tool .

MODE:=debug

.PHONY: default
default: prebuild
	castle-engine compile

.PHONY: prebuild
prebuild:
	castle-engine auto-generate-textures

.PHONY: standalone
standalone: prebuild
	castle-engine --mode=$(MODE) compile $(CASTLE_ENGINE_TOOL_OPTIONS)

.PHONY: clean
clean:
	castle-engine clean

.PHONY: win32
win32: prebuild
	castle-engine --mode=$(MODE) package --os=win32 --cpu=i386

.PHONY: win64
win64: prebuild
	castle-engine --mode=$(MODE) package --os=win64 --cpu=x86_64

.PHONY: linux32
linux32: prebuild
	castle-engine --mode=$(MODE) package --os=linux --cpu=i386

.PHONY: linux64
linux64: prebuild
	castle-engine --mode=$(MODE) package --os=linux --cpu=x86_64

.PHONY: src
src:
	castle-engine --mode=$(MODE) package-source

.PHONY: android
android: prebuild
	castle-engine --mode=$(MODE) package --target=android --fast
	castle-engine install --target=android
	castle-engine run --target=android

.PHONY: android-release
android-release: prebuild
	castle-engine clean
	$(MAKE) android MODE=release

.PHONY: ios
ios: prebuild
	castle-engine --mode=$(MODE) package --target=iOS --fast

.PHONY: ios-release
ios-release: prebuild
	castle-engine clean
	$(MAKE) ios MODE=release
