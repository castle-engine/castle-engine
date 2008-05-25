(Almost-auto)-generating of X3D nodes declarations in Pascal:

- The subdir "1" contains all X3D node's specification, simply pasted from
  X3D specification. I decided it's easier to just paste it and
  then process with simple Pascal program than to dive into
  processing X3D Schema with XSLT. (I pasted all in 30 minutes,
  and that's it...)

  Subdir 1 contains content directly pasted from X3D spec.
  Only the local trivial modifications were done to process it easier:
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

- Subdir 2 is more processed version of 1:
  Everything that can be obtained through inheritance
  (from normal classes, not interfaces) is removed.
  Also various small errors in X3D spec fixed (non-existing X3DColorObject
  replaced with X3DColorNode, also some missing fields (available
  through inheritance, but not listed in spec explicitly) will
  be available in our engine --- that's the idea of X3D spec anyway).

  This is what will be processed to produce actual Pascal code.
