.PHONY: standalone
standalone:
	castle-engine compile $(CASTLE_ENGINE_TOOL_OPTIONS)

.PHONY: release-android
release-android:
	castle-engine package --os=android --cpu=arm
	castle-engine install --os=android --cpu=arm
