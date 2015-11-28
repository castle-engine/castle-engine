# GameAnalytics Android.mk file.
# This will be added to the main integrated/jni/Android.mk file by the build tool.

include $(CLEAR_VARS)
LOCAL_MODULE := libGameAnalytics
LOCAL_SRC_FILES := $(TARGET_ARCH_ABI)/libGameAnalytics.so
include $(PREBUILT_SHARED_LIBRARY)
