# Castle Game Engine - GetIt version

## Instructions

### Before Package Installation

This repo (repo_dirname) needs to be placed in e.g. 

`C:\Users\Public\Documents\Embarcadero\Studio\22.0\CatalogRepository`
or
`C:\Users\[USERNAME]\Documents\Embarcadero\Studio\22.0\CatalogRepository`

The 22.0 part of the path is mutable - 22,0 is the Delphi 11 specific path - it'll change for other versions. This path is referred to by Delphi as $(BDSCatalogRepository). 

A new entry is REQUIRED  called CGEDIR in Tools -> Options -> IDE -> Environment Variables -> User System Overrides, it's value should be $(BDSCatalogRepository)\\(repo_dirname)

This needs performing before the package is compiled/installed as $(CGEDIR) is used within the package.

### Package Installation

Build Win64 Debug for castle_engine

Build Win64 Release for castle_engine

Build Win32 Debug for castle_engine

Install Win32 Release for castle_engine

Install Win32 Release for castle_engine_design

### After Package Installation, before use

Append to Win32 Library path ...

$(CGEDIR)\lib\Win32\Release

Append to Win32 DCU path ...

$(CGEDIR)\lib\Win32\Debug

Append to Win64 Library path ...

$(CGEDIR)\lib\Win64\Release

Append to Win64 DCU path ...

$(CGEDIR)\lib\Win64\Debug

## RESTART DELPHI
