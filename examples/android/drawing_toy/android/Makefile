.PHONY: release
release: data
	cd ../../../../ && ppcrossarm -Tandroid -CfVFPV3 -dRELEASE ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg examples/android/drawing_toy/drawing_toy.lpr
	mkdir -p libs/armeabi/
	mv -f ../libdrawing_toy.so libs/armeabi/libdrawing_toy.so
	ant debug
	adb install -r bin/DrawingToy-debug.apk
	adb shell am start -a android.intent.action.MAIN -n net.sourceforge.castleengine.drawingtoy/android.app.NativeActivity

# Debug version of the app with debug information
.PHONY: debug
debug: data
	cd ../../../../ && ppcrossarm -Tandroid -CfVFPV3 -dDEBUG ${CASTLE_FPC_OPTIONS:-} @castle-fpc.cfg examples/android/drawing_toy/drawing_toy.lpr
	mkdir -p libs/armeabi/
	mv -f ../libdrawing_toy.so jni/libdrawing_toy.so
	export NDK_DEBUG=1 && ndk-build
	ant debug
	adb install -r bin/DrawingToy-debug.apk
	adb shell am start -a android.intent.action.MAIN -n net.sourceforge.castleengine.drawingtoy/android.app.NativeActivity

.PHONY: clean
clean:
	rm -Rf bin/ gen/ libs/ obj/ assets/ jni/libdrawing_toy.so

.PHONY: data
data:
	rm -Rf assets/
	cp -R ../data/ assets/
	find assets/ -type f \
	   '(' -iname '*~' -or \
	       -iname '*.blend1' -or \
	       -iname '*.blend2' ')' \
	   -exec rm -f '{}' ';'
