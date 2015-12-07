.PHONY: standalone
standalone:
	castle-engine compile $(CASTLE_ENGINE_TOOL_OPTIONS)

# Tip: add "--verbose --leave-temp" to the "castle-engine package ..."
# command-line, to see what's going on under the hood:)

.PHONY: release-android
release-android:
	castle-engine package --os=android --cpu=arm
	castle-engine install --os=android --cpu=arm
	castle-engine run --os=android --cpu=arm
