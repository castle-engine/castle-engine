# Vampyre Imaging Library (bundled with Castle Game Engine)

Subdirectory src contains a local copy of https://github.com/galfar/imaginglib .
It is provided here just for comfort of Castle Game Engine developers:
so that, after downloading CGE (from zip or GitHub, and without using git submodules),
Vampyre is immediately available.

The reason is that we depend on Vampyre to provide support for
a number of image formats in CGE. E.g. it is necessary for JPG support in Delphi.
With time, we want to depend on Vampyre even more, as it seems great -- efficient,
cross-platform, for both FPC and Delphi.

You can always run

```
./sync.sh
```

to get here latest Vampyre source code.
