The nodes-specification/ subdirectory contains X3D nodes description
in a simple text format (similar to what is used in X3D specifications).
This is an input to the x3d-nodes-to-pascal program.

x3d-nodes-to-pascal

1. Generates some helper methods and properties,
   and places them in the ../../../src/x3d/auto_generated_node_helpers/
   directory.
   These files are never edited manually.
   So you can rerun x3d-nodes-to-pascal as often as you want.
   Like this:

   castle-engine compile
   ./run.sh

2. x3d-nodes-to-pascal can also generate "starting templates" to define
   an Object Pascal class that defines given X3D node.
   These "starting templates" were used when creating the X3DNodes unit,
   but they require manual adjustment,
   and we try to limit their usage in the future.

------------------------------------------------------------------------------
Historical notes how the initial files in nodes-specification/ were created:

- Originally, this was created by hand, simply pasting from X3D specification.

  (It was much easier to just paste it and then process with simple Pascal program
  than to dive into processing X3D Schema with XSLT.)

  Some modifications were done to process it easier:
  Unicode symbols replaced with "Inf", "Pi" and "-".
  Every field on a single line.
  "# And any number of" minimized to a single line.

- Next step was to decide which X3D node classes have to be implemented as
  Pascal interfaces. Preferably, these should have as few fields as possible,
  so that the interfaces act only to check "is" relationships, and casting
  to interfaces is not necessary (giving current interfaces state in FPC,
  casting "as" interfaces can be troublesome;
  more important, class inheritance allows
  me to reuse implementation, so it's preferred).

  Looking at X3D inheritance graph, this is easy: X3D classes marked
  as (xxx)* have to be Pascal interfaces.

  Nodes marked as "XxxObject" are Pascal interfaces, and only
  Pascal interfaces.

  I decided to not change X3D inheritance
  (For example, I could make X3DMetadataObject a normal
  class, descending from X3DNode, and stil keep all "is"
  relationships. But, for the future, it's better to stick to X3D inheritance.)

  Classes marked as "XxxNode" that are never mentioned as
  secondary ancestor are only Pascal classes.

  Classes marked sometimes as secondary and sometimes as primary ancestors
  need to be implemented as both Pascal classes and interfaces.
  The distinction between primary/secondary is arbitrary (actually,
  just like written in X3D spec, but this is actually alphabetical...).
  For example, X3DGroupingNode and X3DSensorNode are both valid
  ancestors for various nodes. Only time will tell which one is more
  suitable as ancestor to implement...

- In the next step, everything that can be obtained through inheritance
  (from normal classes, not interfaces) is removed.
  Also various small errors in X3D spec fixed (non-existing X3DColorObject
  replaced with X3DColorNode, also some missing fields (available
  through inheritance, but not listed in spec explicitly) will
  be available in our engine --- that's the idea of X3D spec anyway).

  This is what will be processed to produce actual Pascal code.
