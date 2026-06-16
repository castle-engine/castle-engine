# Build Delphi projects on command-line: dcc or msbuild?

## dcc

This is our historic approach in Castle Game Engine build tool, no longer recommended.

It worked nicely to build for Windows, but it is not good to build for other platforms, where we would need to add a lot of platform-specific options.

For example, for Linux one needs to add this, as of Delphi 11.3, to allow the compiler to find and link Linux libraries (in SDK, i.e. imported from your system, and FMXLinux stuff):

```
-UC:\Users\Public\Documents\Embarcadero\Studio\22.0\CatalogRepository\FmxLinux-1.74\source
-UC:\Users\Public\Documents\Embarcadero\Studio\22.0\CatalogRepository\FmxLinux-1.74\lib\Release
--syslibroot:C:\Users\michalis\Documents\Embarcadero\Studio\SDKs\ubuntu23.10.sdk\
--libpath:C:\Users\michalis\Documents\Embarcadero\Studio\SDKs\ubuntu23.10.sdk\usr\lib\gcc\x86_64-linux-gnu\10;C:\Users\michalis\Documents\Embarcadero\Studio\SDKs\ubuntu23.10.sdk\usr\lib\x86_64-linux-gnu;C:\Users\michalis\Documents\Embarcadero\Studio\SDKs\ubuntu23.10.sdk\lib\x86_64-linux-gnu;C:\Users\michalis\Documents\Embarcadero\Studio\SDKs\ubuntu23.10.sdk\lib64;C:\Program Files (x86)\Embarcadero\Studio\22.0\lib\linux64\release
```

You can find these paths in Delphi IDE:

- FMXLinux is in "Tools → Options → Language → Delphi → Library Path".

- Libraries are in "Tools → Options → Deployment → SDK Manager → <your Linux connection> → Local root directory".

  They may use environment variables, also defined locally in Delphi IDE,
  you'll have to expand them. You can also just take a look at the dcclinux64...
  command-line used by Delphi IDE when you compile a project for Linux from
  Delphi: it will contain also the correct --syslibroot etc.

- Add one additional library location inside Delphi installation,
  C:\Program Files (x86)\Embarcadero\Studio\22.0\lib\linux64\release above.

## msbuild

Using msbuild instead is much easier and "just works".
See https://docwiki.embarcadero.com/RADStudio/Florence/en/Building_a_Project_Using_an_MSBuild_Command

One just needs to use this from the `cmd` command-line:

```
call "C:/Program Files (x86)/Embarcadero/Studio/<delphi_version>/bin/rsvars.bat"
msbuild my_project.dproj /p:platform=Linux64
```

To execute above from an application, you can create 2-line `bat` script with above commands, and execute it with `cmd`.