.PHONY: default
default: data
	cd ../ && sh compile.sh
	mkdir -p libs/armeabi/
	cp -f ../libandroiddemo.so libs/armeabi/libandroiddemo.so
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
