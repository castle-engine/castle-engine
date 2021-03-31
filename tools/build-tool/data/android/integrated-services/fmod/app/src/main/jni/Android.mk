# This will be added to the main integrated/jni/Android.mk file by the build tool.

include $(CLEAR_VARS)
LOCAL_MODULE := fmod
LOCAL_SRC_FILES := $(TARGET_ARCH_ABI)/libfmod.so
include $(PREBUILT_SHARED_LIBRARY)
