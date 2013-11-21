.PHONY: default
default:
	cd ../ && sh compile.sh
	mkdir -p libs/armeabi/
	cp -f ../libandroiddemo.so libs/armeabi/libandroiddemo.so
	ant debug
	adb install -r bin/AndroidDemo-debug.apk
	adb shell am start -a android.intent.action.MAIN -n net.sourceforge.castleengine.androiddemo/android.app.NativeActivity

.PHONY: clean
clean:
	rm -Rf bin/ gen/ libs/ obj/

.PHONY: data
data:
	adb push ../data/ /sdcard/castle_game_engine/androiddemo/
