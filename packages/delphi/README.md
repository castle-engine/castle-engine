Delphi packages.

Build and Install them all easily by a group AllPackages.groupproj.

Notes:

- To install in Delphi, be sure to choose "Windows 32-bit" platform, not "Windows 64-bit".
  Delphi IDE seems to be 32-bit.
  If you're on "Windows 64-bit" you will not even see the menu options to "Install" packages.

- We put DCP output in ./output subdirectory, this seems simplest and reliable for now.
  Makes it easier to find and refer to packages than in directories like
  (default) C:\Users\Public\Documents\Embarcadero\Studio\22.0\Dcp .

- Package settings disable some warnings.
  Unfortunately we cannot do it by including castleconf.inc (that already does this job)
  or even writing directives in dpk file -- it seems that when compiling dpk,
  some dumber preprocessor is being used.

    { Disable Delphi warnings that we're not compatible with C++.
      TODO: In the future we plan to support C++ builder, so these will have to be dealt with. }
    {$warn DUPLICATE_CTOR_DTOR off}
    {$warn UNSUPPORTED_CONSTRUCT}

    { Disable Delphi warnings that we import more units implicitly.
      We do it deliberately, to avoid listing huge number of units in castle_base. }
    {$warn IMPLICIT_IMPORT off}
