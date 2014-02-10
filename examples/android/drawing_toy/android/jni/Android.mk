LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)
LOCAL_MODULE := libdrawing_toy
LOCAL_SRC_FILES := libdrawing_toy.so
include $(PREBUILT_SHARED_LIBRARY)
