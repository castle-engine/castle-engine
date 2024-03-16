# Check that Castle Game Engine Lazarus (.lpk) and Delphi (.dpk, .dproj) packages are correct

In particular check that they contain all files they should, and none of the files they shouldn't. This checks:

- The packages are complete. Missing files from packages, while not critical, is bothersome E.g. Lazarus will not auto-recompile package if you change a file that wasn't explicitly listed in .lpk.

- The package doesn't use something it should not. E.g. castle_base.lpk should not use any files from src/window/, as they depend on CastleWindow, and castle_base.lpk deliberately doesn't depend on CastleWindow.

See https://castle-engine.io/units_map about CGE subdirectories.

Using [Castle Game Engine](https://castle-engine.io/).

## Adjusting the tool for your own projects

While this is an internal CGE tool, it could be useful for other projects as well. The tool can be compiled with both <em>FPC</em> and <em>Delphi</em> (and regardless of the compiler, it can check both <em>Lazarus</em> and <em>Delphi</em> packages).

The core part happens in the main program, that defines what packages to check are requirements to watch:

```delphi
Package := TLazarusPackage.Create('foo.lpk');
try
  Package.CheckFiles([
    'src/required_units/', // dir
    'src/specific_required_unit.pas', // file
    'src/required*.pas' // files, as mask
  ],
  [
    'src/required_units/exceptions_to_not_include/', // dir
    'src/required_units/specific_exception_to_not_include.pas' // file
  ],
  [
    'src/optional_units/', // dir
    'src/specific_optional_unit.pas' // file
  ]);
finally FreeAndNil(Package) end;

Package := TDelphiPackage.Create('bar.dpk');
try
  Package.CheckFiles([
    'src/required_units/', // dir
    'src/specific_required_unit.pas' // file
  ],
  [
    'src/required_units/exceptions_to_not_include/', // dir
    'src/required_units/specific_exception_to_not_include.pas'
  ],
  [
    'src/optional_units/', // dir
    'src/specific_optional_unit.pas' // file
  ]);
finally FreeAndNil(Package) end;
```

You may be able to just copy this project, and adjust the main piece to check your own packages.

## Building

Compile by:

- [CGE editor](https://castle-engine.io/editor). Just use menu items _"Compile"_ or _"Compile And Run"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `check_packages.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/lazarus).

- Or use [Delphi](https://www.embarcadero.com/products/Delphi). Open in Delphi `check_packages.dproj` file and compile / run from Delphi. See [CGE and Delphi](https://castle-engine.io/delphi) documentation for details.