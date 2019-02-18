# This will be added to the main integrated/jni/Android.mk file by the build tool.

# libtremolo.so from https://github.com/castle-engine/android-tremolo

include $(CLEAR_VARS)
LOCAL_MODULE := libtremolo
LOCAL_SRC_FILES := $(TARGET_ARCH_ABI)/libtremolo.so
# Use these lines for the "low precision" version
# LOCAL_MODULE := libtremolo-low-precision
# LOCAL_SRC_FILES := $(TARGET_ARCH_ABI)/libtremolo-low-precision.so
include $(PREBUILT_SHARED_LIBRARY)
