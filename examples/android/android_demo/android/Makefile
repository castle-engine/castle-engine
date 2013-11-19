.PHONY: default
default:
	mkdir -p libs/armeabi/
	cp -f ../libcge_android_lib.so libs/armeabi/libcge_android_lib.so
	ant debug
	adb install -r bin/CgeAndroidTest-debug.apk
	adb shell am start -a android.intent.action.MAIN -n net.sourceforge.castleengine.cgeandroidtest/android.app.NativeActivity

.PHONY: clean
clean:
	rm -Rf bin/ gen/ libs/ obj/
