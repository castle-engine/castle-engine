# Delphi package

This directory contains a design-time Delphi package `castle_engine` that registers [TCastleControl component you can drop on a form (VCL, FMX)](https://castle-engine.io/control_on_form).

We recommend installing this package using [Delphinus](https://castle-engine.io/download#delphinus). It will install the package and extend your Delphi settings, to make CGE units available for all applications.

Alternatively, you can build and install this package manually in Delphi IDE.

Notes:

- The package supports only _"Windows 32-bit"_ platform, as Delphi IDE is 32-bit right now, so _"Windows 32-bit"_ is the only platform that makes sense for design-time packages (that can be installed, that can depend on `designide`).

  _Castle Game Engine_ itself supports both 32-bit and 64-bit Windows. And we actually recommend to build your applications for Windows-64, as this what users expect nowadays.

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

    This worked when installing in Delphi IDE, however [Delphinus](https://castle-engine.io/download#delphinus) has issues installing packages that depend on other packages like this. So we decided to switch to a single Delphi package `castle_engine` that contains everything, both VCL and FMX components. There seems to be no practical disadvantages of this.

    In case we'll want to use run-time packages, they will be split into separate base/fmx/vcl.
