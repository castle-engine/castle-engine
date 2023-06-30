# Delphi package

This directory contains a design-time Delphi package `castle_engine` that registers [TCastleControl component you can drop on a form (VCL, FMX)](https://castle-engine.io/control_on_form).

We recommend installing this package using [Delphinus](https://castle-engine.io/download#delphinus). It will install the package and extend your Delphi settings, to make CGE units available for all applications.

Alternatively, you can build and install this package manually in Delphi IDE.

Notes:

- The package `castle_engine` _"Target platforms"_ must include all platforms supported by Castle Game Engine with Delphi.

    This means both _"Windows 32-bit"_ and _"Windows 64-bit"_. This allows to drop `TCastleControl` on a form when your application platform is either _"Windows 32-bit"_ or _"Windows 64-bit".

    We generally recommend to build your Windows applications for 64-bit, as this is what users expect nowadays. For maximum compatibility, CGE supports both 32-bit and 64-bit Windows versions, with any compiler.

- However, `castle_engine_design` package _"Target platforms"_ must be only Win32. Because `designide` package is only for Windows 32-bit, just like Delphi IDE.

    And `designide` includes unit `ToolsAPI` which is in turn used by `CastleInternalDelphiDesignUtils` unit.

- As Delphi IDE is 32-bit right now, note that you can use _"Install"_ on a package only when the platform is set to _"Windows 32-bit"_.

- We put DCP output in `./output` subdirectory, this seems simplest and reliable for now.
  Makes it easier to find and refer to packages than in directories like
  (default) `C:\Users\Public\Documents\Embarcadero\Studio\22.0\Dcp` .

- Package settings disable some warnings.
  Disabling them this way (not in DPK, e.g. by including `castleconf.inc` or writing directives explicitly) seems like the only reliable way to hide them. The directives (and reasons for hiding) are:

    ```
    { Disable Delphi warnings that we're not compatible with C++.
      TODO: In the future we plan to support C++ builder, so these will have to be dealt with. }
    {$warn DUPLICATE_CTOR_DTOR off}
    {$warn UNSUPPORTED_CONSTRUCT}

    { Disable Delphi warnings that we import more units implicitly.
      We do it deliberately, to avoid listing huge number of units in castle_base. }
    {$warn IMPLICIT_IMPORT off}
    ```

- We used to have here a set of packages:

    - `castle_base` with base units
    - `castle_fmx` with FMX version of TCastleControl, that depended on `castle_base`
    - `castle_vcl` with VCL version of TCastleControl, that depended on `castle_base`

    It is possible we'll go back to this split,

    - When adding non-Windows platforms (Linux, Android) that will have FMX components but not VCL components.

    - In case we'll want to use run-time packages, they will need to be split into separate base/FMX/VCL, as a normal application doesn't use both FMX and VCL simultaneously, so it should not depend on both.

- `src\vampyre_imaginglib\src\Source\JpegLib\imjidctasm.pas` is implicitly imported into package -- this is normal and has to stay like this. This unit compiles only for Delphi/Win32 (not Delphi/Win64), it is also used only in case of that platform.

- The packages are not useful with Delphinus at this point, due to https://github.com/Memnarch/Delphinus/issues/93 .
