LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)
LOCAL_MODULE := libandroiddemo
LOCAL_SRC_FILES := libandroiddemo.so
include $(PREBUILT_SHARED_LIBRARY)
