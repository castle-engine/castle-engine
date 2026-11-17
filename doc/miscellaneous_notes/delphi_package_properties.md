# Using CASTLE_ENGINE_PACKAGES_PROPERTIES environment variable to customize how Delphi builds the engine packages

## Introduction

All _Castle Game Engine_ Delphi packages (`packages/delphi/*.dproj`) import an additional _MSBuild_ properties file, if the environment variable `CASTLE_ENGINE_PACKAGES_PROPERTIES` is defined and points to an existing file.

This allows you to change how the engine packages are built (e.g. pass additional compiler options), without modifying the engine sources. The properties file can be maintained (and committed) in your own project repository.

If `CASTLE_ENGINE_PACKAGES_PROPERTIES` is not defined, is empty, or points to a non-existing file, nothing happens -- the packages build as usual.

## How to define the environment variable

Do one of the following:

- Define it in Delphi IDE: _Tools -> Options -> IDE -> Environment Variables_, in the _User System Overrides_ section click _New..._, and set:

    - Variable name: `CASTLE_ENGINE_PACKAGES_PROPERTIES`
    - Variable value: full path to your properties file, like `C:\my-project\castle_engine_packages.props`.

    This affects builds from the Delphi IDE. No IDE restart is necessary.

- Or define it as a regular Windows environment variable. This affects both Delphi IDE (restart the IDE after changing it) and command-line builds using `msbuild`.

## Using with command-line msbuild

When building from the command-line, you can also pass it as an MSBuild property: `msbuild ... /p:CASTLE_ENGINE_PACKAGES_PROPERTIES=C:\my-project\castle_engine_packages.props`.

Note: the variable defined in the Delphi IDE _User System Overrides_ is not visible to command-line `msbuild` (even after calling `rsvars.bat`).

## How it works

Each `packages/delphi/*.dproj` contains, just before importing `CodeGear.Delphi.Targets`:

```xml
<Import Project="$(CASTLE_ENGINE_PACKAGES_PROPERTIES)" Condition="'$(CASTLE_ENGINE_PACKAGES_PROPERTIES)'!='' And Exists('$(CASTLE_ENGINE_PACKAGES_PROPERTIES)')"/>
```

Since the import is after all the project's own `PropertyGroup` sections, properties set in your file override the project settings. And since it is before `CodeGear.Delphi.Targets`, they affect how the compiler is called.

## Sample use-case: build all engine packages as "never build" (like `{$IMPLICITBUILD OFF}`)

Our packages use `{$IMPLICITBUILD ON}` in the `.dpk` sources, which is the Delphi default.

However, Delphi doesn't allow a package with `{$IMPLICITBUILD OFF}` ("never build" package) to require a package with `{$IMPLICITBUILD ON}` ("always build" package). If your own package has `{$IMPLICITBUILD OFF}` and requires e.g. `castle_engine`, compiling it fails with:

```
error E2466: Never-build package 'MyPackage' requires always-build package 'castle_engine'
```

To solve this, create a properties file like this:

```xml
<?xml version="1.0" encoding="utf-8"?>
<Project xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
  <PropertyGroup>
    <DCC_OutputNeverBuildDcps>true</DCC_OutputNeverBuildDcps>
  </PropertyGroup>
</Project>
```

and point `CASTLE_ENGINE_PACKAGES_PROPERTIES` to it. Then rebuild the engine packages.

This works because `DCC_OutputNeverBuildDcps=true` makes the compiler called with the `-Z` command-line option ("Output 'never build' DCPs"). The `-Z` option overrides the `{$IMPLICITBUILD ON}` in the `.dpk` source code, so all engine packages are built as "never build" packages, as if they had `{$IMPLICITBUILD OFF}`. Your never-build packages can then require them.
