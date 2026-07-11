# We disable Msys2 path conversions

Reasons behind setting

- `MSYS2_ENV_CONV_EXCL`
- `MSYS2_ARG_CONV_EXCL`
- `MSYS_NO_PATHCONV`

in our workflow file (`test-and-pack-runner-native.yml`) and various scripts (like `pack_release.sh`):

MSys2 does something quite unexpected (IMHO) by default: it autoconverts any parameter that looks like Unix path to Windows path. See https://www.msys2.org/docs/filesystem-paths/ .

This does result in weird problems when calling `iscc`, `signtool` or anything else that expects parameters starting with `/`.

I know why they did it, but I still call it a mistake: this is very surprising (see below), "mangling" your parameters and environment before it reaches the target program.

When reported in bash through `set -x` everything looks fine, it seems we call:

```
iscc
  'D:\a\castle-engine\castle-engine/tools/internal/pack_release/cge-windows-setup.iss'
  /OD:/a/castle-engine/castle-engine
  /Fcastle-engine-setup-7.0-alpha.snapshot
  /DMyAppSrcDir=C:/Users/RUNNER~1/AppData/Local/Temp/castle-engine-release-1378/castle_game_engine
  /DMyAppVersion=7.0-alpha.snapshot
```

(see `iscc` command-line docs). But in reality, MSys2 `bash` is passing (detected by
our https://github.com/michaliskambi/report-params/ ) these nonsense parameters:

```
iscc
  D:\a\castle-engine\castle-engine/tools/internal/pack_release/cge-windows-setup.iss
  C:\Program Files\Git\OD;A:\castle-engine\castle-engine
  C:/Program Files/Git/Fcastle-engine-setup-7.0-alpha.snapshot
  C:\Program Files\Git\DMyAppSrcDir=C;C:\Program Files\Git\Users\RUNNER~1\AppData\Local\Temp\castle-engine-release-1378\castle_game_engine
  C:/Program Files/Git/DMyAppVersion=7.0-alpha.snapshot
```

As you see, it added `C:/Program Files/Git/` or `C:\Program Files\Git\` prefix
in front of every parameter that started with `/`.
This of course breaks `iscc` later.
We don't want this.

## MSYS_NO_PATHCONV

Git For Windows has a different name `MSYS_NO_PATHCONV` for above:
- https://stackoverflow.com/questions/7250130/how-to-stop-mingw-and-msys-from-mangling-path-names-given-at-the-command-line
- https://github.com/git-for-windows/git/issues/577
- https://github.com/git-for-windows/msys2-runtime/pull/11

So we set both `MSYS2_ENV_CONV_EXCL` and `MSYS2_ARG_CONV_EXCL` (for MSys2) and
`MSYS_NO_PATHCONV` (for Git For Windows) to disable this "feature".

## Better way: use cygpath explicitly

Our scripts are prepared to call `cygpath` to deal
with path conversion problems explicitly. It is much much more reliable,
and it is explicit -- easy to debug.
