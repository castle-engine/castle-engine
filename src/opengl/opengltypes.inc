{$ifdef read_interface}

type
  TDynGLuintArray = TDynLongWordArray;

  TVector2f = TVector2Single;   PVector2f = PVector2Single;
  TVector2d = TVector2Double;   PVector2d = PVector2Double;
  TVector2ub = TVector2Byte;    PVector2ub = PVector2Byte;
  TVector2i = TVector2LongInt;  PVector2i = PVector2LongInt;

  TVector3f = TVector3Single;   PVector3f = PVector3Single;
  TVector3d = TVector3Double;   PVector3d = PVector3Double;
  TVector3ub = TVector3Byte;    PVector3ub = PVector3Byte;
  TVector3i = TVector3LongInt;  PVector3i = PVector3LongInt;

  TVector4f = TVector4Single;   PVector4f = PVector4Single;
  TVector4d = TVector4Double;   PVector4d = PVector4Double;
  TVector4ub = TVector4Byte;    PVector4ub = PVector4Byte;
  TVector4i = TVector4LongInt;  PVector4i = PVector4LongInt;

  TMatrix2i = TMatrix2Longint; PMatrix2i = PMatrix2Longint;
  TMatrix2f = TMatrix2Single;  PMatrix2f = PMatrix2Single;
  TMatrix2d = TMatrix2Double;  PMatrix2d = PMatrix2Double;

  TMatrix3i = TMatrix3Longint; PMatrix3i = PMatrix3Longint;
  TMatrix3f = TMatrix3Single;  PMatrix3f = PMatrix3Single;
  TMatrix3d = TMatrix3Double;  PMatrix3d = PMatrix3Double;

  TMatrix4i = TMatrix4Longint; PMatrix4i = PMatrix4Longint;
  TMatrix4f = TMatrix4Single;  PMatrix4f = PMatrix4Single;
  TMatrix4d = TMatrix4Double;  PMatrix4d = PMatrix4Double;

const
  DefaultMatAmbientColor4ub : TVector4Byte = (51, 51, 51, 255);
  DefaultMatAmbientColor4f : TVector4Single = (0.2, 0.2, 0.2, 1.0);
  DefaultMatDiffuseColor4ub : TVector4Byte = (204, 204, 204, 255);
  DefaultMatDiffuseColor4f : TVector4Single = (0.8, 0.8, 0.8, 1.0);
  DefaultMatSpecularColor4ub : TVector4Byte = (0, 0, 0, 255);
  DefaultMatSpecularColor4f : TVector4Single = (0, 0, 0, 1.0);

type
  { more geometry types } { }

  TPolygonStipple = packed array[0..(32*32 div 8)-1]of TGLubyte;
  PPolygonStipple = ^TPolygonStipple;

  TTriangle3f = TTriangle3Single;
  TTriangle3d = TTriangle3Double;

  { infinite arrays } { }

  TArray_GLuint = packed array[0..MaxInt div SizeOf(TGLuint)-1]of TGLuint;
  PArray_GLuint = ^TArray_GLuint;
  TArray_Vector3d = packed array[0..MaxInt div SizeOf(TVector3d)-1]of TVector3d;
  PArray_Vector3d = ^TArray_Vector3d;
  TArray_Vector3ub = packed array[0..MaxInt div SizeOf(TVector3ub)-1]of TVector3ub;
  PArray_Vector3ub = ^TArray_Vector3ub;

{$endif}

{$ifdef read_implementation}

{$endif}
