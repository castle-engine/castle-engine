{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Access WebGL API from Castle Game Engine compiled in WebAssembly.

  The API is deliberately made to be compatible CastleGLES unit,
  to use this unit as a "drop-in replacement" as much as possible.

  But note that we don't try to force 100% compatibility with CastleGLES,
  in some cases we leave things incompatible and rendering code has to do
  "ifdef CASTLE_WEBGL". We mostly leave things incompatible when it seems
  we would lose some type safety (e.g. C API wants to use pointers for some
  uniform data, WebGL is sometimes nicer in this regard).
  We also in some cases decided to expose for OpenGL/OpenGLES an API
  to make them consistent with WebGL: this applies to
  castleinternalglutils_create_delete.inc . }
unit CastleInternalWebGL;

interface

uses JOB.JS, Classes,
  CastleInternalJobWeb, CastleVectors, CastleUtils;

type
  { Alias simple types to CastleInternalJobWeb types. }
  TGLenum = CastleInternalJobWeb.TGLenum;
  TGLboolean = CastleInternalJobWeb.TGLboolean;
  TGLbitfield = CastleInternalJobWeb.TGLbitfield;
  TGLbyte = CastleInternalJobWeb.TGLbyte;
  TGLshort = CastleInternalJobWeb.TGLshort;
  TGLint = CastleInternalJobWeb.TGLint;
  TGLsizei = CastleInternalJobWeb.TGLsizei;
  TGLintptr = CastleInternalJobWeb.TGLintptr;
  TGLsizeiptr = CastleInternalJobWeb.TGLsizeiptr;
  TGLubyte = CastleInternalJobWeb.TGLubyte;
  TGLushort = CastleInternalJobWeb.TGLushort;
  TGLuint = CastleInternalJobWeb.TGLuint;
  TGLfloat = CastleInternalJobWeb.TGLfloat;
  TGLclampf = CastleInternalJobWeb.TGLclampf;

const
  { Define GL_TRUE/FALSE for compatibility with CastleGLES.
    In WebGL, as defined in CastleInternalJobWeb, these are just Pascal booleans. }
  GL_TRUE = true;
  GL_FALSE = false;

{$define read_interface}
{$I castleinternalwebgl_flat_api.inc}
{$undef read_interface}

{ Routines providing WebGL "flat" API that looks deliberately similar to
  CastleGLES unit. }

const
  { WebGL has only STENCIL_INDEX8, expose more generic name STENCIL_INDEX.
    See https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/renderbufferStorage }
  GL_STENCIL_INDEX = GL_STENCIL_INDEX8;

function glGetString(const Param: TGLenum): String;
procedure glBufferData(const Target: TGLenum; const Size: PtrUInt; const Data: Pointer; const Usage: TGLenum);
procedure glBufferSubData(const Target: TGLenum; const DstOffset: TGLintptr; const Size: PtrUInt; const Data: Pointer);
procedure glDrawElements(const mode: TGLenum; const count: TGLsizei; const type_: TGLenum; const offset: Pointer);

{ Helper routines specific to WebGL }

var
  GL: IJSWebGLRenderingContext;
  GL2: IJSWebGL2RenderingContext;

  { All WebGL extensions. Always non-nil if GL is non-nil,
    so WebGL context was initialized. }
  WebGLExtensions: TStrings;

{ Initialize WebGLExtensions. }
procedure ReadExtensions;

{ Convert various types to WebGL arrays, e.g. to provide uniform value to shaders. }

function MatrixToWebGL(const M: TMatrix2): IJSFloat32Array;
function MatrixToWebGL(const M: TMatrix3): IJSFloat32Array;
function MatrixToWebGL(const M: TMatrix4): IJSFloat32Array;

function ListToWebGL(const L: CastleUtils.TInt32List): IJSInt32Array;
function ListToWebGL(const L: CastleUtils.TSingleList): IJSFloat32Array;
function ListToWebGL(const L: TVector2List): IJSFloat32Array;
function ListToWebGL(const L: TVector3List): IJSFloat32Array;
function ListToWebGL(const L: TVector4List): IJSFloat32Array;
function ListToWebGL(const L: TMatrix3List): IJSFloat32Array;
function ListToWebGL(const L: TMatrix4List): IJSFloat32Array;
function ListToWebGL(const Values: array of String): IJSArray;

implementation

uses SysUtils, JOB.Shared,
  CastleLog, CastleStringUtils,
  // TODO: web: not necessary in the future? only for some test queries below
  CastleInternalGLUtils;

{$push}
  // Hide: Warning: Implicit string type conversion from "AnsiString" to "UnicodeString"
  {$warn 4105 off}
  // Hide: Warning: Implicit string type conversion with potential data loss from "UnicodeString" to "AnsiString"
  {$warn 4104 off}

  {$define read_implementation}
  {$I castleinternalwebgl_flat_api.inc}
  {$undef read_implementation}
{$pop}

function glGetString(const Param: TGLenum): String;
begin
  Result := GL.GetParameter(Param);
end;

function MemoryToWebGL(const Ptr: Pointer; const Size: PtrUInt): IJSArrayBuffer; forward;

procedure glBufferData(const Target: TGLenum;
  const Size: PtrUInt; const Data: Pointer; const Usage: TGLenum);
begin
  glBufferData(Target, MemoryToWebGL(Data, Size), Usage);
end;

procedure glBufferSubData(const Target: TGLenum; const DstOffset: TGLintptr;
  const Size: PtrUInt; const Data: Pointer);
begin
  glBufferSubData(Target, DstOffset, MemoryToWebGL(Data, Size));
end;

procedure glDrawElements(const mode: TGLenum; const count: TGLsizei;
  const type_: TGLenum; const offset: Pointer);
begin
  glDrawElements(mode, count, type_, PtrUInt(offset));
end;

{ Rendering and animation using WebGL ---------------------------------------- }

procedure ReadExtensions;
var
  A: CastleInternalJobWeb.TUnicodeStringDynArray;
  I: Integer;
  Ext: String;
begin
  A := GL.getSupportedExtensions;
  WebGLExtensions := TStringList.Create;
  for I := 0 to A.Length - 1 do
  begin
    // TODO: web: TJSArray._GetStrings is not OK, see https://gitlab.com/freepascal.org/fpc/source/-/merge_requests/893
    // Ext := A.Strings[I];
    // Workaround:
    Ext := PrefixSuffixRemove('"', '"', A.Elements[I].AsString, false);
    WebGLExtensions.Add(Ext);
  end;

  WritelnLog('Supported Extensions: ' + NL +
    '  Count: %d' + NL +
    '  List: %s', [
    WebGLExtensions.Count,
    GlueStrings(WebGLExtensions, ', ')
  ]);
end;

procedure MatrixToWebGLAdd(const M: TMatrix2; const List: IJSFloat32Array; const Offset: Integer);
var
  Col, Row: TMatrix2.TIndex;
begin
  { Convert to a list of values in column-major order, as expected by WebGL:
    https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/uniformMatrix }
  for Col := Low(Col) to High(Col) do
    for Row := Low(Row) to High(Row) do
      List[Offset + Col * 2 + Row] := M[Col, Row];
end;

procedure MatrixToWebGLAdd(const M: TMatrix3; const List: IJSFloat32Array; const Offset: Integer);
var
  Col, Row: TMatrix3.TIndex;
begin
  { Convert to a list of values in column-major order, as expected by WebGL:
    https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/uniformMatrix }
  for Col := Low(Col) to High(Col) do
    for Row := Low(Row) to High(Row) do
      List[Offset + Col * 3 + Row] := M[Col, Row];
end;

procedure MatrixToWebGLAdd(const M: TMatrix4; const List: IJSFloat32Array; const Offset: Integer);
var
  Col, Row: TMatrix4.TIndex;
begin
  { Convert to a list of values in column-major order, as expected by WebGL:
    https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/uniformMatrix }
  for Col := Low(Col) to High(Col) do
    for Row := Low(Row) to High(Row) do
      List[Offset + Col * 4 + Row] := M[Col, Row];
end;

function MatrixToWebGL(const M: TMatrix2): IJSFloat32Array;
begin
  Result := TJSFloat32Array.Create(2 * 2);
  MatrixToWebGLAdd(M, Result, 0);
end;

function MatrixToWebGL(const M: TMatrix3): IJSFloat32Array;
begin
  Result := TJSFloat32Array.Create(3 * 3);
  MatrixToWebGLAdd(M, Result, 0);
end;

function MatrixToWebGL(const M: TMatrix4): IJSFloat32Array;
begin
  Result := TJSFloat32Array.Create(4 * 4);
  MatrixToWebGLAdd(M, Result, 0);
end;

function ListToWebGL(const L: CastleUtils.TInt32List): IJSInt32Array;
var
  I: Integer;
begin
  Result := TJSInt32Array.Create(L.Count);
  for I := 0 to L.Count - 1 do
    Result[I] := L[I];
end;

function ListToWebGL(const L: CastleUtils.TSingleList): IJSFloat32Array;
var
  I: Integer;
begin
  Result := TJSFloat32Array.Create(L.Count);
  for I := 0 to L.Count - 1 do
    Result[I] := L[I];
end;

function ListToWebGL(const L: TVector2List): IJSFloat32Array;
var
  I: Integer;
begin
  Result := TJSFloat32Array.Create(L.Count * 2);
  for I := 0 to L.Count - 1 do
  begin
    Result[I * 2] := L[I].Data[0];
    Result[I * 2 + 1] := L[I].Data[1];
  end;
end;

function ListToWebGL(const L: TVector3List): IJSFloat32Array;
var
  I: Integer;
begin
  Result := TJSFloat32Array.Create(L.Count * 3);
  for I := 0 to L.Count - 1 do
  begin
    Result[I * 3] := L[I].Data[0];
    Result[I * 3 + 1] := L[I].Data[1];
    Result[I * 3 + 2] := L[I].Data[2];
  end;
end;

function ListToWebGL(const L: TVector4List): IJSFloat32Array;
var
  I: Integer;
begin
  Result := TJSFloat32Array.Create(L.Count * 4);
  for I := 0 to L.Count - 1 do
  begin
    Result[I * 4] := L[I].Data[0];
    Result[I * 4 + 1] := L[I].Data[1];
    Result[I * 4 + 2] := L[I].Data[2];
    Result[I * 4 + 3] := L[I].Data[3];
  end;
end;

function ListToWebGL(const L: TMatrix3List): IJSFloat32Array;
var
  I: Integer;
begin
  Result := TJSFloat32Array.Create(L.Count * 9);
  for I := 0 to L.Count - 1 do
    MatrixToWebGLAdd(L[I], Result, I * 9);
end;

function ListToWebGL(const L: TMatrix4List): IJSFloat32Array;
var
  I: Integer;
begin
  Result := TJSFloat32Array.Create(L.Count * 16);
  for I := 0 to L.Count - 1 do
    MatrixToWebGLAdd(L[I], Result, I * 16);
end;

function ListToWebGL(const Values: array of String): IJSArray;
var
  I: Integer;
begin
  Result := TJSArray.Create([]);
  for I := 0 to High(Values) do
    Result.Push(Values[I]);
end;

function MemoryToWebGL(const Ptr: Pointer; const Size: PtrUInt): IJSArrayBuffer;
begin
  Result := TJSArrayBuffer.Create(Size);
  Result.CopyFromMemory(Ptr, Size);
end;

finalization
  FreeAndNil(WebGLExtensions);
end.
