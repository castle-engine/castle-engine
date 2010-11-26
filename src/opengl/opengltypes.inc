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

  { Dyn arrays and some "infinite" static arrays. } { }

  TDynVector2fArray = TDynVector2SingleArray;
  TArray_Vector2f = TArray_Vector2Single;
  PArray_Vector2f = PArray_Vector2Single;

  TDynVector3fArray = TDynVector3SingleArray;
  TArray_Vector3f = TArray_Vector3Single;
  PArray_Vector3f = PArray_Vector3Single;

{ Trivial operations on types below } { }

function Vector2f(x, y: TGLfloat): TVector2f;
function Vector3f(x, y: TGLfloat; z: TGLfloat {$IFDEF DEFPARS}=0.0{$ENDIF}): TVector3f; overload;
function Vector3f(const v3: TVector3d): TVector3f; overload;
function Vector3f(const v3: TVector3ub): TVector3f; overload;
function Vector3d(x, y: TGLdouble; z: TGLdouble {$IFDEF DEFPARS}=0.0{$ENDIF}): TVector3d; overload;
function Vector3d(const v: TVector3f): TVector3d; overload;
function Vector4f(x, y: TGLfloat; z: TGLfloat {$IFDEF DEFPARS}=0{$ENDIF}; w: TGLfloat {$IFDEF DEFPARS}=1.0{$ENDIF}): TVector4f; overload;
function Vector4f(const v3: TVector3f; w: TGLfloat {$IFDEF DEFPARS}=1.0{$ENDIF}): TVector4f; overload;
function Vector4f(const ub: TVector4ub): TVector4f; overload;

function Vector3ub(x, y, z: TGLubyte): TVector3ub;
function Vector4ub(x, y, z, w: TGLubyte): TVector4ub; overload;
function Vector4ub(const f4: TVector4f): TVector4ub; overload;

{$endif}

{$ifdef read_implementation}

function Vector2f(x, y: TGLfloat): TVector2f;
begin result[0] := x; result[1] := y end;

function Vector4f(x, y: TGLfloat; z: TGLfloat{=0}; w: TGLfloat{=1}): TVector4f;
begin
 result[0] := x; result[1] := y; result[2] := z; result[3] := w;
end;

function Vector4f(const v3: TVector3f; w: TGLfloat{=1}): TVector4f;
begin
 move(v3, result, SizeOf(TVector3f)); result[3] := w;
end;

function Vector4f(const ub: TVector4ub): TVector4f;
begin
 result[0] := ub[0]/255;
 result[1] := ub[1]/255;
 result[2] := ub[2]/255;
 result[3] := ub[3]/255;
end;

function Vector3f(x, y: TGLfloat; z: TGLfloat{=0.0}): TVector3f;
begin
 result[0] := x; result[1] := y; result[2] := z;
end;

function Vector3d(x, y: TGLdouble; z: TGLdouble{=0.0}): TVector3d;
begin
 result[0] := x; result[1] := y; result[2] := z;
end;

function Vector3f(const v3: TVector3d): TVector3f;
begin
 result[0] := v3[0]; result[1] := v3[1]; result[2] := v3[2];
end;

function Vector3f(const v3: TVector3ub): TVector3f; overload;
begin
 result[0] := v3[0]/255;
 result[1] := v3[1]/255;
 result[2] := v3[2]/255;
end;

function Vector3d(const v: TVector3f): TVector3d;
begin
 result[0] := v[0]; result[1] := v[1]; result[2] := v[2];
end;

function Vector3ub(x, y, z: TGLubyte): TVector3ub;
begin
 result[0] := x; result[1] := y; result[2] := z;
end;

function Vector4ub(x, y, z, w: TGLubyte): TVector4ub;
begin
 result[0] := x; result[1] := y; result[2] := z; result[3] := w;
end;

function Vector4ub(const f4: TVector4f): TVector4ub;
begin
 result[0] := Round(f4[0]*255);
 result[1] := Round(f4[1]*255);
 result[2] := Round(f4[2]*255);
 result[3] := Round(f4[3]*255);
end;

{$endif}
