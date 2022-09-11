Copy of GL and GLExt units from FPC 3.2.2 with a simple hack:
Tolerate (set to nil all entry points) the situation when OpenGL dynamic library
is not available on the system.

This is 1 practical use-case for this so far:

You can use these units with build tool, that uses GL,GLExt units
(because that was easier, right now some CGE units indirectly depend on GL,GLExt
even though they should not) to run the build tool even on systems without OpenGL
library installed.
For example the "shared runners" made available by GitLab for Windows
do not have OpenGL library. If using standard GL,GLExt units,
any CGE application (even build tool that doesn't of course initialize any OpenGL
context) would thus fail.

So using these units is useful to make GitLab CI for Windows working, see
https://castle-engine.io/gitlab_ci
https://gitlab.com/castle-engine/test-gitlab-ci/ .

In the future, when the renderer will be pluggable,
the need for this hack should disappear. Build tool should not use GL,GLExt units at all.
