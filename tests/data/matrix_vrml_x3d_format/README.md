# Testing matrix load / save format in VRML 1.0, 2.0 and X3D

VRML 1.0 and X3D specifications differ in how to load / save matrix.

- VRML 1.0 says, for `SFMatrix` field:

  """
  SFMatrices are written to file in row-major order as 16 floating point
  numbers separated by whitespace.
  For example, a matrix expressing a translation of 7.3 units
  along the X axis is written as:
  1 0 0 0  0 1 0 0  0 0 1 0  7.3 0 0 1
  """

  https://paulbourke.net/dataformats/vrml1/#Fields

- X3D implies the opposite notation for its `SFMatrix3f`, `SFMatrix4f`, `SFMatrix3d` and `SFMatrix4d` fields:

  """
  The first four single-precision floating point numbers represent
  the top row of the matrix.
  The second four single-precision floating point numbers represent
  the second row of the matrix.
  """

  Classic encoding:

  https://www.web3d.org/documents/specifications/19776-2/V3.3/Part02/EncodingOfFields.html#SFMatrix4f

  XML encoding:

  https://www.web3d.org/documents/specifications/19776-1/V3.3/Part01/EncodingOfFields.html#SFMatrix4f

To complicate matters:

- VRML 2.0 (97) doesn't define SFMatrix in spec.
  Though some implementations, including ours,
  allow MatrixTransform node as an extension, with SFMatrix field.

  https://graphics.stanford.edu/courses/cs248-98-fall/Assignments/Assignment3/VRML2_Specification/spec/part1/fieldsRef.html

  We decided to use VRML 1.0 format for VRML 2.0.

- There are not many nodes actually using `SFMatrix*` or `MFMatrix*` fields.

  Looking at the X3D standard, there are really only 4:
  - `RigidBody.inertia` (SFMatrix3f)
  - `TextureTransformMatrix3D.matrix` (SFMatrix4f)
  - `Matrix3VertexAttribute` and `Matrix4VertexAttribute` (MFMatrix3f, MFMatrix4f)

  And the scripts, including shaders, may define own fields of matrix type.

  Some notable extensions use this however:
  - `MatrixTransform.matrix`
  - `Skin.inverseBindMatrices` ( https://castle-engine.io/skin )
  - `RenderedTexture.viewing` and `RenderedTexture.projection`

See the `MatrixFormatPerRow` routine for code that decides which format to use.

This directory contains testcases of this.
