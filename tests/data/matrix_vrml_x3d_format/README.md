# Testing matrix load / save format in VRML 1.0, 2.0 and X3D

## How specs say to store the matrix

VRML 1.0 and X3D specifications say how to load / save matrix.

- VRML 1.0 says, for `SFMatrix` field:

  """
  SFMatrices are written to file in row-major order as 16 floating point
  numbers separated by whitespace.
  For example, a matrix expressing a translation of 7.3 units
  along the X axis is written as:
  1 0 0 0  0 1 0 0  0 0 1 0  7.3 0 0 1
  """

  https://paulbourke.net/dataformats/vrml1/#Fields

- X3D says this about `SFMatrix3f`, `SFMatrix4f`, `SFMatrix3d` and `SFMatrix4d` fields:

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

  But also X3D 3 and X3D 4 both say: _"Since these data types are commonly used for transformation matrices, translation values are stored in the fourth row."_

  - https://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/fieldsDef.html#SFMatrix4fAndMFMatrix4f

  - https://www.web3d.org/specifications/X3Dv4/ISO-IEC19775-1v4-IS/Part01/fieldTypes.html#SFMatrix4fAndMFMatrix4f

## Conclusions

The specs prose mean that:

- X3D matrix meaning is "transposed" compared the CGE (and common mathematics) convention, because in CGE translation is stored in the last column (not last row) of `TMatrix4`.

- VRML 1.0 and X3D specs match.

- The way X3DOM handles `MatrixTransform.matrix` is correct with regards to the X3D standard.

## Testcases

- (X3D XML) https://github.com/castle-engine/demo-models/blob/master/x3d/matrix_transform.x3d

- (X3D classic) https://github.com/castle-engine/demo-models/blob/master/x3d/matrix_transform.x3dv

- (X3DOM) https://github.com/castle-engine/demo-models/blob/master/x3d/matrix_transform_x3dom.html

## Which nodes use it

To complicate testing, the matrix field types are used very seldom:

- VRML 2.0 (97) doesn't define `SFMatrix` in spec at all.
  Though some implementations, including ours,
  allow `MatrixTransform` node as an extension, with `SFMatrix` field.

  https://graphics.stanford.edu/courses/cs248-98-fall/Assignments/Assignment3/VRML2_Specification/spec/part1/fieldsRef.html

- There are not many nodes actually using `SFMatrix*` or `MFMatrix*` fields in X3D.

  Looking at the X3D standard, there are really only 4:
  - `RigidBody.inertia` (SFMatrix3f)
  - `TextureTransformMatrix3D.matrix` (SFMatrix4f)
  - `Matrix3VertexAttribute` and `Matrix4VertexAttribute` (MFMatrix3f, MFMatrix4f)

  And the scripts, including shaders, may define own fields of matrix type.

  Some notable extensions use this however:
  - `MatrixTransform.matrix`
  - `Skin.inverseBindMatrices` ( https://castle-engine.io/skin )
  - `RenderedTexture.viewing` and `RenderedTexture.projection`

## Pascal autotests

This directory contains testcases of this for CGE Pascal auto-tests.
