# To release run like
#   make BUILD=release
# to build in release mode. Make sure ant.properties contains lines like
#   key.store=.../michalis.keystore
#   key.alias=michalis
# (where "michalis" is my own nick, use your own :)
# See http://developer.android.com/tools/publishing/app-signing.html

BUILD=debug

.PHONY: release
release: data
	cd ../ && castle-engine compile --mode=release --os=android --cpu=arm
	mkdir -p libs/armeabi/
	mv -f ../libandroiddemo.so libs/armeabi/libandroiddemo.so
	ant $(BUILD)
	adb install -r bin/AndroidDemo-debug.apk
	adb shell am start -a android.intent.action.MAIN -n net.sourceforge.castleengine.androiddemo/android.app.NativeActivity

# Debug version of the app with debug information
.PHONY: debug
debug: data
	cd ../ && castle-engine compile --mode=debug --os=android --cpu=arm
	mkdir -p libs/armeabi/
	mv -f ../libandroiddemo.so jni/libandroiddemo.so
	export NDK_DEBUG=1 && ndk-build
	ant debug
	adb install -r bin/AndroidDemo-debug.apk
	adb shell am start -a android.intent.action.MAIN -n net.sourceforge.castleengine.androiddemo/android.app.NativeActivity

.PHONY: clean
clean:
	rm -Rf bin/ gen/ libs/ obj/ assets/ jni/libandroiddemo.so

.PHONY: data
data:
	rm -Rf assets/
	cp -R ../data/ assets/
	find assets/ -type f \
	   '(' -iname '*~' -or \
	       -iname '*.blend1' -or \
	       -iname '*.blend2' ')' \
	   -exec rm -f '{}' ';'
