# This will be added to the main integrated/jni/Android.mk file by the build tool.

# libtremolo.so from https://github.com/michaliskambi/tremolo-android

include $(CLEAR_VARS)
LOCAL_MODULE := libtremolo
LOCAL_SRC_FILES := libtremolo.so
include $(PREBUILT_SHARED_LIBRARY)
