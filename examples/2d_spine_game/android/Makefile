# To release run like
#   make BUILD=release
# to build in release mode. Make sure ant.properties contains
#   key.store=.../michalis.keystore
#   key.alias=michalis
# See http://developer.android.com/tools/publishing/app-signing.html

BUILD=debug

.PHONY: default
default: data icons
	cd ../ && castle-engine compile --mode=release --os=android --cpu=arm
	mkdir -p libs/armeabi/
	cp -f ../code/libcastle_spine_android.so libs/armeabi/libcastle_spine_android.so
	ant $(BUILD)
#	adb uninstall net.sourceforge.castleengine.castlespine
	adb install -r bin/CastleSpine-$(BUILD).apk
	adb shell am start -a android.intent.action.MAIN -n net.sourceforge.castleengine.castlespine/android.app.NativeActivity

.PHONY: data
data:
	rm -Rf assets/
	cp -R ../data/ assets/
	find assets/ -type f \
	   '(' -iname '*~' -or \
	       -iname '*.xcf' -or \
	       -iname '*.blend' ')' \
	   -exec rm -f '{}' ';'

.PHONY: clean
clean:
	rm -Rf bin/ gen/ libs/ obj/ assets/ ../code/libcastle_spine_android.so

# Autogenerate ic_launcher based on ../store-graphics/icon.png
ICON_SOURCE:=../store-graphics/icon.png
.PHONY: icons
icons:
	mkdir -p res/drawable-ldpi/ \
	         res/drawable-mdpi/ \
		 res/drawable-hdpi/ \
		 res/drawable-xhdpi/ \
		 res/drawable-xxhdpi/
	convert $(ICON_SOURCE) -geometry 36x36   res/drawable-ldpi/ic_launcher.png
	convert $(ICON_SOURCE) -geometry 48x48   res/drawable-mdpi/ic_launcher.png
	convert $(ICON_SOURCE) -geometry 72x72   res/drawable-hdpi/ic_launcher.png
	convert $(ICON_SOURCE) -geometry 96x96   res/drawable-xhdpi/ic_launcher.png
	convert $(ICON_SOURCE) -geometry 144x144 res/drawable-xxhdpi/ic_launcher.png
