# Build Delphi projects on command-line: dcc or msbuild?

## 1. Directly calling `dcc*` (default for Windows)

This approach works nicely to build for Windows. Our [build tool](https://castle-engine.io/build_tool) constructs the command-line for `dcc` (Delphi compiler) with all the engine paths, and everything works fine.

### Problems with dcc for non-Windows platforms

Unfortunately, this approach does not reasonably work when targeting non-Windows platforms, where we need to add a lot of platform-specific options to make the `dcc*` command work correctly.

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
  `C:\Program Files (x86)\Embarcadero\Studio\22.0\lib\linux64\release` above.

You can pass all these options to `dcc` using the `--compiler-option` like `castle-engine compile --compiler=delphi "--compiler-option=-UC:\Users\Public\Documents\...."`. But this is very cumbersome, both finding the right options and passing them to the build tool.

## 2. Using `msbuild` (default for non-Windows)

Using msbuild instead is much easier for non-Windows platforms and "just works".
See https://docwiki.embarcadero.com/RADStudio/Florence/en/Building_a_Project_Using_an_MSBuild_Command

One just needs to use this from the `cmd` command-line:

```
call "C:/Program Files (x86)/Embarcadero/Studio/<delphi_version>/bin/rsvars.bat"
msbuild my_project.dproj /p:platform=Linux64
```

To execute above from an application, you can create 2-line `bat` script with above commands, and execute it with `cmd`.

Under the hood, `msbuild` will call `dcc` with all the correct options for given platform.

### 32k command-line limit of msbuild

Unfortunately, `msbuild` has problems with the command length. When we pass a lot of paths to `DCC_UnitSearchPath` (and we do, as we provide all engine paths) the command-line gets too long causing warning (followed by an error) like this:

```
warning MSB6002: The command-line for the "DCC" task is too long. Command-lines longer than 32000 characters are likely to fail. Try reducing the length of the command-line by breaking down the call to "DCC" into multiple calls with fewer parameters per call.
```

This happens with both old (10.2) and new (13.1) Delphi versions.

( We can *not* avoid this problem by using environment references like `$(CGESRC)` in the `DCC_UnitSearchPath`. It seems msbuild expands them before passing to dcc, so we still get too long command-line. )

## Conclusion

For above reasons:

- We use `dcc` approach when building for Windows,

- We use `msbuild` for other platforms. You must put engine in a shorter path then, to avoid hitting the 32k command-line limit.

You can force using `dcc` approach with any platform using the `--use-delphi-dcc` option to `castle-engine compile --compiler=delphi`. Be prepared to pass extra options to `dcc` for non-Windows platforms in this case, using `--compiler-option`, see above.