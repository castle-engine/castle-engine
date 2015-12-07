Smaller templates for project stuff.

Regenerate them using "make" in this dir. This generates xxx.inc files from xxx,
using file_to_pascal_string --- a trivial program
that is part of pasdoc [http://pasdoc.sf.net/] project, see
https://svn.code.sf.net/p/pasdoc/code/trunk/source/tools/file_to_pascal_string.dpr .
The resulting xxx.inc files are then compiled-in in the build tool binary.

Note that "larger templates" are in ../data/ directory, not here,
and they are not compiled-in. Instead, they are simply loaded, at runtime,
by the build tool (from it's ApplicationData() path).

How do we decide which templates go here (small)
and which to ../data/ directory (large)?

- Like the name suggests, larger templates (like Android "integrated" template)
  should go to "large" templates dir, as it's just more convenient to manage
  larger structure of files/dirs there.
  (no need to regenerate their .inc files, and the user can change the templates
  without recompiling build tool).

- However, the templates in ../data/ only work when ApplicationData() is correct.
  We make every effort to make it correct, but it's impossible to make it 100%
  magically always work, if the user does not define $CASTLE_ENGINE_PATH correctly.

  So "crucial" stuff, especially required during "compile" stage (not only
  during "package" stage), can go to "smaller" templates dir here.
  Right now this is the case for Windows resource stuff, necessary at compilation.
