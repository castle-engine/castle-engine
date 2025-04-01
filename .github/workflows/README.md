# Quick version of GitHub Actions that build CGE

These actions only build Windows and Linux releases of CGE, ASAP, without most of the tests.

Use only when you're

- relatively sure all tests pass (like: Delphi compilation, FPC compilation, auto-tests...)

- you only want ASAP builds of CGE for Windows/x86_64 and Linux/x86_64

- you really need release ASAP.

Note: To make it fast, best to make sure the runners are not busy with other jobs: https://github.com/organizations/castle-engine/settings/actions/runners .