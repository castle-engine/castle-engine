# This will be added to the main integrated/jni/Android.mk file by the build tool.

include $(CLEAR_VARS)
LOCAL_MODULE := freetype
LOCAL_SRC_FILES := armeabi-v7a/libfreetype.so
include $(PREBUILT_SHARED_LIBRARY)
