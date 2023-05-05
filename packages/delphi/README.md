# Delphi packages

We recommend installing them using [Delphinus](https://castle-engine.io/download#delphinus). Alternatively, you can build and install them manually.

These are design-time packages that register [TCastleControl component you can drop on a form (VCL, FMX)](https://castle-engine.io/control_on_form).

Notes:

- To install in Delphi, be sure to choose "Windows 32-bit" platform, not "Windows 64-bit".
  Delphi IDE is 32-bit.

  If you're on "Windows 64-bit" you will not even see the menu options to "Install" packages.
  Moreover, as the packages require `designide`, you actually cannot even "Build" them for Windows-64 now.

  Note that _Castle Game Engine_ supports both Windows-32 and Windows-64. Just these packages are only for Windows-32, as they are design-time only and Delphi IDE is 32-bit.

- We put DCP output in `./output` subdirectory, this seems simplest and reliable for now.
  Makes it easier to find and refer to packages than in directories like
  (default) `C:\Users\Public\Documents\Embarcadero\Studio\22.0\Dcp` .

- Package settings disable some warnings.
  Disabling them this way (not in DPK, e.g. by `castleconf.inc` (that already does this job)
  or writing directives explicitly) seems like this only reliable way to hide them.

    ```
    { Disable Delphi warnings that we're not compatible with C++.
      TODO: In the future we plan to support C++ builder, so these will have to be dealt with. }
    {$warn DUPLICATE_CTOR_DTOR off}
    {$warn UNSUPPORTED_CONSTRUCT}

    { Disable Delphi warnings that we import more units implicitly.
      We do it deliberately, to avoid listing huge number of units in castle_base. }
    {$warn IMPLICIT_IMPORT off}
    ```
