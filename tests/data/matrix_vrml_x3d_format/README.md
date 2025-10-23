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
  The first three single-precision floating point numbers represent
  the top row of the matrix.
  The second three single-precision floating point numbers represent
  the second row of the matrix.
  """

  https://www.web3d.org/documents/specifications/19776-2/V3.3/Part02/EncodingOfFields.html#SFMatrix3f

To complicate matters:

- VRML 2.0 (97) doesn't define SFMatrix in spec.
  Though some implementations, including ours,
  allow MatrixTransform node as an extension, with SFMatrix field.

  https://graphics.stanford.edu/courses/cs248-98-fall/Assignments/Assignment3/VRML2_Specification/spec/part1/fieldsRef.html

  We decided to use VRML 1.0 format for VRML 2.0.

See the `MatrixFormatPerRow` routine for code that decides which format to use.

This directory contains testcases of this.
