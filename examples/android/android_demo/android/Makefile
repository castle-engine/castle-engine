.PHONY: release
release: data
	cd ../ && sh compile.sh -dRELEASE
	mkdir -p libs/armeabi/
	cp -f ../libandroiddemo.so libs/armeabi/libandroiddemo.so
	ant debug
	adb install -r bin/AndroidDemo-debug.apk
	adb shell am start -a android.intent.action.MAIN -n net.sourceforge.castleengine.androiddemo/android.app.NativeActivity

# Debug version of the app with debug information
.PHONY: debug
debug: data
	cd ../ && sh compile.sh -dDEBUG
	mkdir -p libs/armeabi/
	cp -f ../libandroiddemo.so jni/libandroiddemo.so
	export NDK_DEBUG=1 && ndk-build
	ant debug
	adb install -r bin/AndroidDemo-debug.apk
	adb shell am start -a android.intent.action.MAIN -n net.sourceforge.castleengine.androiddemo/android.app.NativeActivity

.PHONY: clean
clean:
	rm -Rf bin/ gen/ libs/ obj/ assets/

.PHONY: data
data:
	rm -Rf assets/
	cp -R ../data/ assets/
	find assets/ -type f \
	   '(' -iname '*~' -or \
	       -iname '*.blend1' -or \
	       -iname '*.blend2' ')' \
	   -exec rm -f '{}' ';'
