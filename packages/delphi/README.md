# Delphi package

This directory contains Delphi packages to make Delphi aware of _Castle Game Engine_.

## Packages

- `castle_engine.bpl` (design-time and run-time) should be installed in Delphi IDE. It contains the CGE base units and components, like [TCastleControl component you can drop on a form (VCL, FMX)](https://castle-engine.io/control_on_form).

- `castle_engine_design.bpl` (design-time only) should be installed in Delphi IDE. It contains additional design-time features for CGE components.

- `castle_engine_window.bpl` (run-time only) provides `TCastleWindow` class and friends. It cannot be installed in Delphi IDE. It is a key to using `TCastleWindow` in your own applications, which is a great approach when you want to create a typical game using CGE, with everything (including UI) designed using CGE.

## Installation in Delphi

- Open in Delphi `AllPackages.groupproj`

- Optional: Right-click on _"AllPackages"_ and select _"Build All"_, to make sure all packages are built.

- Make sure your platform is _"Windows 32-bit"_ (Delphi IDE is 32-bit right now, so installed packages must be 32-bit too)

- Right-click on `castle_engine.bpl` and select _"Install"_

- Right-click on `castle_engine_design.bpl` and select _"Install"_

## Notes - why packages are configured like this

- The package `castle_engine` _"Target platforms"_ must include all platforms supported by Castle Game Engine with Delphi.

    This means both _"Windows 32-bit"_ and _"Windows 64-bit"_. This allows to drop `TCastleControl` on a form when your application platform is either _"Windows 32-bit"_ or _"Windows 64-bit".

    We generally recommend to build your Windows applications for 64-bit, as this is what users expect nowadays. For maximum compatibility, CGE supports both 32-bit and 64-bit Windows versions, with any compiler.

- However, `castle_engine_design` package _"Target platforms"_ must be only Win32. Because `designide` package is only for Windows 32-bit, just like Delphi IDE.

    And `designide` includes unit `ToolsAPI` which is in turn used by `CastleInternalDelphiDesignUtils` unit.

- As Delphi IDE is 32-bit right now, note that you can use _"Install"_ on a package only when the platform is set to _"Windows 32-bit"_.

- `castle_engine_window.bpl` has all supported CGE platforms, just like `castle_engine.bpl` .

- `castle_engine_window.bpl` cannot be installed in Delphi IDE, to secure us from an important mistake:

    `TCastleWindow` and `TCastleApplication` inside `CastleWindow` unit may talk to widgetsets directly, e.g. using WinAPI. Doing this could conflict with normal working of Delphi IDE -- there are some things that are essentially set application-wide on the widgetset (like WinAPI) and all libraries (VCL, FMX, and `TCastleWindow`) have to set these things assuming "we are the only library through which user integrates with this widgetset".

    Note: We actually do not talk to widgetsets until `Application` singleton is initialized, and even then we defer any "dangerous" initialization to when it is really necessary (usually creation of `TCastleWindow`, eventually querying e.g. `Application.ScreenWidth`). And from Delphi IDE, neither `TCastleWindow` nor `TCastleApplication` should ever need to be created, nothing should call `Application` making the singleton initialized.

    ... But just in case we'll make a mistake (e.g. `CastleWindow` initialization does, by mistake, some call to WinAPI, maybe indirectly) it is better to keep these units to never be installed in Delphi IDE. They should never be needed at design-time anyway.

- We put output in the default directory determined by Delphi, which will be like `C:\Users\Public\Documents\Embarcadero\Studio\22.0\Bpl` .

- Not relevant anymore:

  Package settings DID disable some warnings.
  Disabling them this way (not in DPK, e.g. by including `castleconf.inc`
  or writing directives explicitly) seemed like the only reliable way to hide them.
  The directives (and reasons for hiding) were:

  ... Not relevant anymore. We no longer disable any warnings.

  - Even C++-related warnings are fine, we honor (fix) them,
    to stay perfectly compatible with C++.

- We used to have here a set of packages:

    - `castle_base` with base units
    - `castle_fmx` with FMX version of TCastleControl, that depended on `castle_base`
    - `castle_vcl` with VCL version of TCastleControl, that depended on `castle_base`

    It is possible we'll go back to this split,

    - When adding non-Windows platforms (Linux, Android) that will have FMX components but not VCL components.

    - In case we'll want to use run-time packages, they will need to be split into separate base/FMX/VCL, as a normal application doesn't use both FMX and VCL simultaneously, so it should not depend on both.

- Note that we don't add deprecated units, from `src/deprecated_units`, like `CastleProgress`, to the Delphi packages. At least for now. This makes building the packages look clean in Delphi IDE -- important, as we don't want new users to be confused by warnings when building packages, warnings about things they'll likely should not care about.

- The packages are not useful with Delphinus at this point, due to https://github.com/Memnarch/Delphinus/issues/93 .

    Hopefully this will be fixed eventually, we'd love to recommend to use install our packages using [Delphinus](https://castle-engine.io/download#delphinus). It should install the package and extend your Delphi settings, to make CGE units available for all applications.

