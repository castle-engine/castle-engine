LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE := lib${ANDROID_LIBRARY_NAME}

# The Android.mk file is parsed once for each architecture
# defined in Application.mk. We can just use $(TARGET_ARCH_ABI)
# for the current architecture name.
# See https://developer.android.com/ndk/guides/android_mk .
LOCAL_SRC_FILES := $(TARGET_ARCH_ABI)/lib${ANDROID_LIBRARY_NAME}.so

include $(PREBUILT_SHARED_LIBRARY)
