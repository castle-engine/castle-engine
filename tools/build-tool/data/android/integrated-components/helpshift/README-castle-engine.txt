TODO: Helpshift will not work right now, it reaches the 64k method limit for Android build toolchain...

To work around it, we need to
- use something called "multi dex",
- *or* limit the google play services to its subset,
Both these things require using the "gradle" build system, not "ant"-based one.
See
http://android-developers.blogspot.co.at/2014/12/google-play-services-and-dex-method.html
http://stackoverflow.com/questions/15209831/unable-to-execute-dex-method-id-not-in-0-0xffff-65536

So finishing Helpshift depends on migrating to building with gradle.

------------------------------------------------------------------------------
Instrucitons for using Helpshift (will be pasted on
https://github.com/castle-engine/castle-engine/wiki/Android-Project-Components-Integrated-with-Castle-Game-Engine )
once we make it work (see above TODO) :

- copy from ~/installed/android/sdk/extras/android/support/v7
  to integrated-components/helpshift/
  - appcompat
  - cardview
  - recyclerview

- copy from ~/installed/android/sdk/extras/android/support/
  to integrated-components/helpshift/
  - design

- for all four folders above,
  - make sure they contain the file project.properties (create if needed)...
  - ...with the line "android.library=true"

- to integrated-components/helpshift/design/project.properties add dependencies:

  android.library.reference.1=../appcompat/
  android.library.reference.2=../cardview/
  android.library.reference.3=../recyclerview/

- copy helpshift-sdk-android (unpacked from zip downloaded from helpshift.com) to integrated-components/helpshift/

- to helpshift-sdk-android/project.properties add:

  android.library=true
  android.library.reference.1=../appcompat/
  android.library.reference.2=../cardview/
  android.library.reference.3=../recyclerview/
  android.library.reference.3=../design/

- move helpshift-sdk-android/assets/hs__data to assets/

- compile with SDK "target-23" or higher
  (probably not needed, as github examples do not say so?)

- change integrated/AndroidManifest.xml to specify
  minSDKVersion 14
  targetSDKVersion 23
  See https://developers.helpshift.com/android/getting-started/
  That is, make this change:

  +
  +      - Helpshift requires min version >= 14, target >= 23.
  +        https://developers.helpshift.com/android/getting-started/
       -->
  -    <uses-sdk android:minSdkVersion="9" android:targetSdkVersion="18" />
  +    <uses-sdk android:minSdkVersion="14" android:targetSdkVersion="23" />

  TODO: we should do it nicer, to allow our internal manifest merger to be able to handle this

- TODO: add castlehelpshift.pas to Lazarus castle_base.lpk
