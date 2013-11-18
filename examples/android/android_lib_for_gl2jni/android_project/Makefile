.PHONY: default
default:
	mkdir -p libs/armeabi/
	cp -f ../libcge_android_lib.so libs/armeabi/libcge_android_lib.so
	ant debug
	adb install -r bin/GL2JNIActivity-debug.apk
	adb shell am start -a android.intent.action.MAIN -n net.sourceforge.castleengine.cgeandroidtest/.GL2JNIActivity

.PHONY: clean
clean:
	rm -Rf bin/ gen/ libs/
