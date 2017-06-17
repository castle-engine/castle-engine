# This will be added to the main integrated/jni/Android.mk file by the build tool.

# libtremolo.so from https://github.com/michaliskambi/tremolo-android

include $(CLEAR_VARS)
LOCAL_MODULE := libtremolo
LOCAL_SRC_FILES := armeabi-v7a/libtremolo.so
# Use these lines for the "low precision" version
# LOCAL_MODULE := libtremolo-low-precision
# LOCAL_SRC_FILES := armeabi-v7a/libtremolo-low-precision.so
include $(PREBUILT_SHARED_LIBRARY)
