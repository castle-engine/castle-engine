# Quick version of GitHub Actions that build CGE

These actions only build Windows/x86_64 and Linux/x86_64 releases of CGE, ASAP, without most of the tests.

How:

- The GHA code in `.github/workflows/` is greatly cut down. But everything else in the engine, including `pack_release.sh`, is unmodified. You can look at branch diff https://github.com/castle-engine/castle-engine/compare/master...quick-snapshot to make sure only these diffs are done.

- Just merge here more master work to update release this way.

Use only when you're:

- Relatively sure all tests pass (like: Delphi compilation, FPC compilation, auto-tests...). It's good to look at GHA results (maybe partial) for the full build (from master branch) before using this, to make sure there's no failure. See https://github.com/castle-engine/castle-engine/actions .

- You really need release ASAP, e.g. to expose a "hotfix" for some critical problem.

- You only need (ASAP) builds of CGE for Windows/x86_64 and Linux/x86_64. These are most common platforms used.

- You're OK that this will not propagate rebuild of other projects (like Castle Model Viewer, or updating Docker images). It will also not update the "snapshot" tag.

Practical usecase: make some important "hotfix" available to users ASAP for most important downloads from https://castle-engine.io/download .

Stats:
- This "quick" build takes now ~28 minutes.
- Which is a win over ~4h 20 mins for the full build, with all checks and all platforms.

Hint: To make it as fast as possible, best to make sure the runners are not busy with other jobs: https://github.com/organizations/castle-engine/settings/actions/runners . Cancel other jobs.