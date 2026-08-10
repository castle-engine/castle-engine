# GTK 3 bindings

This is a fork of GTK3 bindings from Lazarus. Units from LCL are renamed, prefix `Laz` -> changes to `CastleInternal`.

Taken from Lazarus sources,
- at git hash b40d9ded5ac3357211175d68ffcda5e76c7cdc69
- which was latest from the `main` branch on 2026-01-14.

It is automated using the script `synch_gtk3_bindings.sh`.

## Why do we need to fork it?

- _Castle Game Engine_ applications don't depend on Lazarus (including LCL). We can integrate with LCL ([TCastleControl](https://castle-engine.io/control_on_form), [we have Lazarus packages](https://castle-engine.io/lazarus), [our build tool can optionally use Lazarus LPI files when build_using_lazbuild="true"](https://castle-engine.io/project_manifest)) but that's all optional, you can use CGE without Lazarus, managing window using our cross-platform `TCastleWindow`.

    So we cannot just use LCL's GTK3 units.

- FPC decided to not include the GTK3 units in their standard units. Discussion about adding GTK3 stalled in https://gitlab.com/freepascal.org/fpc/source/-/issues/39989 .

- So there's no choice except to fork the units to CGE codebase.
