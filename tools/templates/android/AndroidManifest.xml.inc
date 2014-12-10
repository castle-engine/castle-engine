{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "android/AndroidManifest.xml" }
'<?xml version="1.0" encoding="utf-8"?>' + LineEnding +
'<manifest xmlns:android="http://schemas.android.com/apk/res/android"' + LineEnding +
'    package="${QUALIFIED_NAME}"' + LineEnding +
'    android:versionCode="${VERSION_CODE}"' + LineEnding +
'    android:versionName="${VERSION}"' + LineEnding +
'    android:installLocation="auto">' + LineEnding +
'' + LineEnding +
'    <!-- See http://developer.android.com/reference/android/app/NativeActivity.html' + LineEnding +
'      for example manifest using NativeActivity.' + LineEnding +
'      This project does not have any Java code.  -->' + LineEnding +
'    <application' + LineEnding +
'        android:label="@string/app_name"' + LineEnding +
'    	android:icon="@drawable/ic_launcher"' + LineEnding +
'        android:hasCode="false">' + LineEnding +
'        <activity android:name="android.app.NativeActivity"' + LineEnding +
'                android:theme="@android:style/Theme.NoTitleBar.Fullscreen"' + LineEnding +
'            	android:launchMode="singleTask"' + LineEnding +
'            	android:configChanges="orientation|keyboardHidden"' + LineEnding +
'		android:screenOrientation="${ANDROID_SCREEN_ORIENTATION}">' + LineEnding +
'	    <!-- NativeActivity must know the name of our library with JNI callbacks. -->' + LineEnding +
'            <meta-data android:name="android.app.lib_name"' + LineEnding +
'                       android:value="${ANDROID_LIBRARY_NAME}" />' + LineEnding +
'            <intent-filter>' + LineEnding +
'                <action android:name="android.intent.action.MAIN" />' + LineEnding +
'                <category android:name="android.intent.category.LAUNCHER" />' + LineEnding +
'            </intent-filter>' + LineEnding +
'        </activity>' + LineEnding +
'    </application>' + LineEnding +
'' + LineEnding +
'    <!-- About min version:' + LineEnding +
'      We need OpenGL ES 2.0, which means Android 2.0 (API Level 5) and higher.' + LineEnding +
'      We want also NativeActivity and EGL, which require API level 9 or higher.' + LineEnding +
'    -->' + LineEnding +
'    <uses-sdk' + LineEnding +
'      android:minSdkVersion="9"' + LineEnding +
'      android:targetSdkVersion="18" />' + LineEnding +
'    <uses-feature android:glEsVersion="0x00020000"/>' + LineEnding +
'' + LineEnding +
'    <!-- Note: to write to SD card we would also need these permissions: -->' + LineEnding +
'    <!--' + LineEnding +
'    <uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE"/>' + LineEnding +
'    -->' + LineEnding +
'' + LineEnding +
'    ${ANDROID_SCREEN_ORIENTATION_FEATURE}' + LineEnding +
'</manifest>' + LineEnding +
''
