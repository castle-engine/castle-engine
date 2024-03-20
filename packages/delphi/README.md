# Delphi packages

Installing these packages in Delphi is necessary to [register TCastleControl component, that you can drop on a VCL or FMX form](https://castle-engine.io/control_on_form).

NOTE: These packages are *not* necessary for developing applications using `TCastleWindow`. And `TCastleWindow` is recommended if you want to create a typical game using CGE, with everything (including UI) designed using CGE. Our _"New Project"_ templates and most examples use `TCastleWindow`, not `TCastleControl`.

## Installation in Delphi

This is duplicated in https://castle-engine.io/delphi_packages[Delphi packages installation docs]:

- Open in Delphi `AllPackages.groupproj`

- Right-click on all packages (except `castle_engine_window.bpl`) in succession and click _"Install"_.

**Most users can stop reading at this point. You have [TCastleControl component, that you can drop on a VCL or FMX form](https://castle-engine.io/control_on_form).**

The rest of this package is only for the developers of CGE itself, or if you want to better understand how CGE packages work.

## Optional: Building packages in Delphi IDE, for all platforms

While this is not necessary for installation, you can make sure that all packages build correctly for all platforms you want to use:

- Make sure all packages have platform set to _"Windows 32-bit"_.

  Then right-click on _"AllPackages"_ and select _"Build All"_.

- Switch platform of all packages (except `castle_engine_design.bpl`) to _"Windows 64-bit"_.

  Again, right-click on _"AllPackages"_ and select _"Build All"_.

- Switch platform of all packages (except `castle_engine_design.bpl` and `castle_engine_vcl.bpl`) to _"Linux 64-bit"_. Do this if you want to use link:delphi_linux[Delphi on Linux].

  To make it work, first [add FMXLinux to "Library Paths"](https://castle-engine.io/delphi_linux#library_paths).

  Again, right-click on _"AllPackages"_ and select _"Build All"_.

If anything fails, please [submit a bug](https://github.com/castle-engine/castle-engine/issues).

## Packages ovierview

- `castle_engine.bpl` contains the base engine units. Not dependent on VCL, not dependent on FMX, not using `TCastleWindow`.

- `castle_engine_vcl.bpl` contains the engine code dependent on VCL. In particular it registers [TCastleControl component you can drop on a VCL form](https://castle-engine.io/control_on_form).

- `castle_engine_fmx.bpl` contains the engine code dependent on FMX. In particular it registers [TCastleControl component you can drop on an FMX form](https://castle-engine.io/control_on_form).

- `castle_engine_window.bpl` provides `TCastleWindow` class and friends. It is a key to use `TCastleWindow` in your own applications, which is a great approach when you want to create a typical game using CGE, with everything (including UI) designed using CGE.

- `castle_engine_design.bpl` contains additional design-time features for CGE components.

Run-time and design-time:

- `castle_engine_design.bpl` is design-time only. It only contains things that are useful in Delphi IDE, everything else is in other packages.

- `castle_engine_window.bpl` is run-time only. It cannot be installed in Delphi IDE. See below for an explanation why -- it secures us from an important mistake.

- Other packages are design-time and run-time. You can install them all in Delphi IDE.

Platforms:

- `castle_engine_design.bpl` has to be for Win32 only, as it depends on `designide` package which is only for Win32.

- `castle_engine_vcl.bpl` can be only for platforms supported by VCL, which means Win32 and Win64.

- Other packages are for all platforms supported by CGE (with Delphi), which right now means Win32, Win64, Linux 64-bit.

## Various notes about why packages are configured like this

### Platforms

- To keep our installation instructions simple, we make sure (there's auto-test for this) to keep default platform of all packages set to _"Windows 32-bit"_.

    As Delphi IDE is 32-bit right now, note that you can use _"Install"_ on any package (regardless of target platforms it supports) only when the platform is set to _"Windows 32-bit"_.

    NOTE: Your applications can target all platforms supported by CGE with Delphi: _"Windows 32-bit"_, _"Windows 64-bit"_, _"Linux 64-bit"_. Here you select _"Windows 32-bit"_ just because Delphi IDE is Win32 application.

- The packages that register components (`TCastleControl` for both VCL and FMX) and their dependencies must include all platforms supported by Castle Game Engine with Delphi.

    This means _"Windows 32-bit"_, _"Windows 64-bit"_, _"Linux 64-bit"_. This allows to drop `TCastleControl` on a form when your application platform is set to any platform supported by CGE.

- The `castle_engine_design` package _"Target platforms"_ must be only Win32. Because `designide` package is only for Windows 32-bit, just like Delphi IDE.

    And `designide` includes unit `ToolsAPI` which is in turn used by `CastleInternalDelphiDesign` unit.

- Note about Win32 vs Win64: We generally recommend to build your Windows applications for 64-bit, as this is what users expect nowadays. But we fully support both 32-bit and 64-bit Windows versions, with any compiler.

### Why castle_engine_window not design-time?

- `castle_engine_window.bpl` cannot be installed in Delphi IDE, to secure us from an important mistake:

    `TCastleWindow` and `TCastleApplication` inside `CastleWindow` unit may talk to widgetsets directly, e.g. using WinAPI. Doing this could conflict with normal working of Delphi IDE -- there are some things that are essentially set application-wide on the widgetset (like WinAPI) and all libraries (VCL, FMX, and `TCastleWindow`) have to set these things assuming "we are the only library through which user integrates with this widgetset".

    Note: We actually do not talk to widgetsets until `Application` singleton is initialized, and even then we defer any "dangerous" initialization to when it is really necessary (usually creation of `TCastleWindow`, eventually querying e.g. `Application.ScreenWidth`). And from Delphi IDE, neither `TCastleWindow` nor `TCastleApplication` should ever need to be created, nothing should call `Application` making the singleton initialized.

    ... But just in case we'll make a mistake (e.g. `CastleWindow` initialization does, by mistake, some call to WinAPI, maybe indirectly) it is better to keep these units to never be installed in Delphi IDE. They should never be needed at design-time anyway.

### Dependencies

- VCL stuff has to be in a separate package, since VCL is only for Windows, while CGE supports more platforms (like Linux).

    We also put FMX stuff in a separate package, i.e. `castle_engine.bpl` doesn't depend on FMX or VCL.

    Note that FMX stuff could be, for now, actually merged in `castle_engine.bpl`. So we could merge `castle_engine.bpl` and `castle_engine_fmx.bpl` into 1 package. But:

    - For the future, we want to be prepared for platforms that may not have FMX. In CGE, we can already deal with them (using our `TCastleWindow` which may have various backends, including using direct access to API like WinAPI or GTK or Cocoa, and thus it doesn't require FMX). Actually on Windows our `castle_engine_window.bpl` already doesn't need FMX (it uses WinAPI directly). But on Linux it does (for now).

    - In case we'll want to use run-time packages, we don't want one application to link to both FMX and VCL simultaneously. So at run-time, `castle_engine_vcl.bpl` and its basis `castle_engine.bpl` have to be independent from FMX.

- Right now `castle_engine_window.bpl` depends on FMX and on `castle_engine_fmx.bpl`. But this may not be necessary in the future.

    The dependency is there because link:delphi_linux[Delphi on Linux] currently uses FMX. More precisely, it uses `TCastleWindow` backend called `CASTLE_WINDOW_FORM` which uses `TOpenGLControl` from `Fmx.CastleInternalGLControl` unit (`castle_engine_fmx.bpl`) which in turm uses FMX.

    It is possible we will remove this dependency in the future. Some `TCastleWindow` backends access system APIs (like WinAPI or GTK) directly, wihtout the need for FMX, VCL (or LCL in case of FPC/Lazarus). For now it's not a problem, but we want to keep this possibility open for the future, e.g. to support future platforms that may not have FMX.

### Other notes

- Note that we don't add deprecated units, from `src/deprecated_units` or `src/window/deprecated_units`, like `CastleProgress`, to the Delphi packages. At least for now. This makes building the packages look clean in Delphi IDE -- important, as we don't want new users to be confused by warnings when building packages, warnings about things they'll likely should not care about.

- The packages are not useful with Delphinus at this point, due to https://github.com/Memnarch/Delphinus/issues/93 .

    Hopefully this will be fixed eventually, we'd love to recommend to use install our packages using [Delphinus](https://castle-engine.io/download#delphinus). It should install the package and extend your Delphi settings, to make CGE units available for all applications.

- There is automatic checked of these packages in `tools/internal/check_packages/`. You can run it manually whenever you want, Jenkins and GitHub Actions also run it automatically.

    We also have some checks in `tools/internal/cge_shell_tests` (just a bash script).

- It practically doesn't matter if you build (and install) CGE packages with _Debug_ or _Release_. At design-time, we don't do anything time-consuming, that would be affected by the debug/release settings.

- Crafting the perfect version of DPROJ files to commit to this repository is a bit tricky.

    Delphi likes to add/remove some stuff that is depedent on your Delphi version, and on the platforms you have currently installed in Delphi. We don't want that -- as then everyone would change back and forth some DPROJ sections, depending on platforms and Delphi version of each developer. It would be hard to make sure we continue to work in all Delphi versions and platforms we support.

    So make sure to revert some DPROJ sections, or even revert whole DPROJ files (if you intended no modification, i.e. you don't change compilation settings or list of included files), before committing.

- We put resulting BPL files in the default directory determined by Delphi, using the `$(BDSCOMMONDIR)\Bpl\`.

    Like `<DeployFile LocalName="$(BDSCOMMONDIR)\Bpl\castle_engine_design.bpl" Configuration="Debug" Class="ProjectOutput">` in the DPROJ file. The platforms other than `Win32` are in a platform-specific subdirectory, like `Win64` and `Linux64`. This is consistent with how other Delphi BPLs are installed, e.g. JEDI components.

    Be careful when creating new packages, as by default Delphi will configure it to a hardcoded directory like `C:\Users\Public\Documents\Embarcadero\Studio\22.0\Bpl`, specific to your Delphi version and Windows settings.
