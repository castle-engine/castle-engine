# This will be added to the main integrated/jni/Android.mk file by the build tool.

# Self compiled freelib.so. See https://github.com/cdave1/freetype2-android
# Current version: freetype 2.4.4.

include $(CLEAR_VARS)
LOCAL_MODULE := freetype
LOCAL_SRC_FILES := armeabi-v7a/libfreetype.so
include $(PREBUILT_SHARED_LIBRARY)
