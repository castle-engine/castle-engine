{%MainUnit castleinternalglutils.pas}
{
  Copyright 2016-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Part of CastleGLUtils unit: simple helpers for OpenGL commands. }

{$ifdef read_interface}

{ Get OpenGL / OpenGLES / WebGL value.

  Prefer to use these over directly glGetFloatv etc. functions,
  this way we can fallback on getParameter on WebGL and also check
  whether it returns both expected type and expected number of values.

  Guarantee that result is zero in case of OpenGL error.
  (Otherwise, OpenGL could leave them undefined and only set glGetError.)
  @groupBegin }
function glGetFloat(const Param: TGLEnum): TGLfloat;
function glGetFloat2(const Param: TGLEnum): TVector2;

function glGetInteger(const Param: TGLEnum): TGLint;
function glGetInteger2(const Param: TGLEnum): TVector2Integer;

function glGetBoolean(const Param: TGLEnum): TGLboolean;
{ @groupEnd }

(*------------------------------------------------------------------------------
  Comfortable wrappers around many OpenGL functions.
  Overloaded for our vector types.

  Note that functions here simply call appropriate OpenGL functions.
  Long time ago we tried using tricks to speed this up (eliminate
  function call overhead), by importing these functions from so/dll
  under different names, like

    procedure glVertexv(const V: TVector3); cdecl / stdcall;
      overload; external OpenGLDLL name 'glVertex3fv';

  But this was problematic: it assumes that TVector3 will be passed
  by reference. Which actually is not guaranteed by a compiler (FPC sometimes
  doesn't). In newer FPC versions, we could use "constref" for this.
  Or we could just declare these functions as "inline".
  However, speeding these functions is just not needed at all anymore
  (especially with new VBO renderer that passes everything through arrays).

  TODO: Some of these functions should be treated as internal.
  A subset of these internal functions are only available in fixed-function pipeline.
  The rest of these functions are just deprecated -- because they
  are only for fixed-function pipeline, in immediate mode (and all modern code
  should use VBO).
*)

{$ifndef OpenGLES}

// Do not mark glColorv as "deprecated" for now. These have valid (although internal) usage.
{ }
procedure glColorv(const v: TVector3Byte); overload; //deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleAbstractFont.Print(X,Y,Color,...) or TDrawableImage.Color';
{ }
procedure glColorv(const v: TVector4Byte); overload; //deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleAbstractFont.Print(X,Y,Color,...) or TDrawableImage.Color';
{ }
procedure glColorv(const v: TVector3); overload; //deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleAbstractFont.Print(X,Y,Color,...) or TDrawableImage.Color';
{ }
procedure glColorv(const v: TVector4); overload; //deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleAbstractFont.Print(X,Y,Color,...) or TDrawableImage.Color';

{ }
procedure CastleGlClipPlane(plane: TGLenum; const V: TVector4Double);

procedure glNormalv(const v: TVector3); overload;

procedure glMaterialv(face, Param: TGLEnum; const params: TVector4); overload;

procedure glVertexv(const v: TVector2); overload;
procedure glVertexv(const v: TVector3); overload;
procedure glVertexv(const v: TVector4); overload;

procedure glTexCoordv(const v: TVector2); overload;
procedure glTexCoordv(const v: TVector3); overload;
procedure glTexCoordv(const v: TVector4); overload;

procedure glTexGenv(coord, Param: TGLenum; const params: TVector4); overload;

procedure glLightv(light, Param: TGLEnum; const params: TVector4); overload;
procedure glLightv(light, Param: TGLEnum; const params: TVector3); overload;

procedure glLightModelv(Param: TGLenum; const params: TVector4); overload;

procedure glFogv(Param: TGLEnum; const params: TVector4); overload;

// Do not mark as "deprecated" for now. These have valid (although internal) usage.
{ }
procedure glMultMatrix(const m: TMatrix4); overload; //deprecated 'use other methods to transform things, e.g. TCastleUserInterface position / anchors, or TCastleTransform for TCastleScene, or TTransformNode in X3D';
{ }
procedure glLoadMatrix(const m: TMatrix4); overload; //deprecated 'use other methods to transform things, e.g. TCastleUserInterface position / anchors, or TCastleTransform for TCastleScene, or TTransformNode in X3D';

{ }
procedure glTexEnvv(target, Param: TGLEnum; const params: TVector4); overload;

{$endif}

type
  TGLIndex = {$ifdef GLIndexesShort} UInt16 {$else} UInt32 {$endif};
  TGLIndexList = {$ifdef GLIndexesShort} TUInt16List {$else} TUInt32List {$endif};
const
  GLIndexConst = {$ifdef GLIndexesShort} GL_UNSIGNED_SHORT {$else} GL_UNSIGNED_INT {$endif};

var
  { Current color, set by glColorv and used for TCastleAbstractFont font printing
    (in case you use deprecated TCastleAbstractFont.Print overloads without
    explicit colors).

    @deprecated Instead of this, use drawing routines that take
    Color from parameters or properties, like TCastleAbstractFont.Print(X,Y,Color,...)
    or TDrawableImage.Color. }
  CurrentColor: TCastleColor
    deprecated 'instead of this, use drawing routines that take Color from parameters or properties, like TCastleAbstractFont.Print(X,Y,Color,...) or TDrawableImage.Color';

{ Enable or disable given OpenGL flag. }
procedure GLSetEnabled(Value: TGLenum; IsEnabled: boolean);

{ Global position for drawing 2D stuff.
  @deprecated Do this use this.
  Instead of this, use drawing routines that take position as parameters,
  like TDrawableImage.Draw(X,Y) or TCastleAbstractFont.Print(X,Y,...). }
var
  WindowPos: TVector2Integer
    deprecated 'use drawing routines that take position as parameters, like TDrawableImage.Draw(X,Y) or TCastleAbstractFont.Print(X,Y,...)';

type
  TEnableTextureTarget = (etNone, et2D, etCubeMap, et3D);

{ Enable exactly one (or none, for Target=etNone) OpenGL texture target
  for fixed-function pipeline.
  Always use this instead of manually calling @code(glDisable(GL_TEXTURE_2D)),
  @code(glEnable(GL_TEXTURE_2D)) and such. This makes sure to have at most
  one texture target enabled, and disable others.

  Remember that this state is different for every texture unit in OpenGL,
  in case you use multi-texturing.

  Remember that not all texture targets are guaranteed to be supported by OpenGL.
  Target=etNone and Target=et2D are always supported.
  For the rest, check appropriate GLFeatures property (before even
  creating a texture with such type). If you pass an unsupported target type
  to this procedure, it will be ignored (all targets will be disabled,
  like for Target=etNone).

  Note that this is only for fixed-function OpenGL pipeline.
  Shader pipeline completely ignores the enabled state of texture units. }
function GLEnableTexture(const Target: TEnableTextureTarget): boolean;

function BoolToGL(const Value: boolean): TGLboolean;

{$endif read_interface}

{$ifdef read_implementation}

{ glGetXxx ---------------------------------------------------------------------}

function glGetFloat(const Param: TGLEnum): TGLfloat;
{$ifdef CASTLE_WEBGL}
var
  ResultVariant: Variant;
{$endif}
begin
  {$ifdef CASTLE_WEBGL}
  ResultVariant := GL.GetParameter(Param);
  if VarIsEmpty(ResultVariant) then
    raise Exception.CreateFmt('Unexpected VarIsEmpty for glGetFloat(%d)', [
      Param
    ]);
  if VarIsNull(ResultVariant) then
    raise Exception.CreateFmt('Unexpected VarIsNull for glGetFloat(%d)', [
      Param
    ]);
  Result := ResultVariant;
  {$else}
  FillChar(Result, SizeOf(Result), 0);
  glGetFloatv(Param, @Result)
  {$endif}
end;

function glGetFloat2(const Param: TGLenum): TVector2;
{$ifdef CASTLE_WEBGL}
var
  ResultVariant: Variant;
  ResultArray: IJSFloat32Array;
  I: Integer;
begin
  { TODO: web: This fails with error 
  
    in Firefox:
      Running Webassembly resulted in exception. Exception class:  object , message: indirect call signature mismatch
    in Vivaldi:
      Running Webassembly resulted in exception. Exception class:  object , message: null function or function signature mismatch
      
    Unknown why. Same for glGetInteger2.
    
    We definitely use this with correct parameters for GL.getParameter,
    https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/getParameter ,
    for ALIASED_LINE_WIDTH_RANGE -> "Float32Array (with 2 elements)",
    for MAX_VIEWPORT_DIMS -> "Int32Array (with 2 elements)".
    
    But all ways to get values fail. Using
    
    var
      ResultArray: IJSFloat32Array;
    begin
      ResultArray := TJSFloat32Array.Cast(GL.GetParameter(Param));
      
    ... should make sense according to 
    https://wiki.freepascal.org/WebAssembly/DOM#Variants . 
    Checks with VarIsEmpty, VarIsNull seem OK (it is not empty, not null).
    
    Checking TJSObject.InvokeJSVariantResult also shows it is OK,
    result is JOBResult_Object.   
    
    TODO:
    - Create minimal testcase, with own JS code returning Int32Array.
      Submit a bug to FPC if still fails.
    - As a better workaround, we could likely do this from pas2js application,
      and just pass the result. For now, using this is not very critical.
  }
  Exit(Vector2(1, 4)); // sensible result of ALIASED_LINE_WIDTH_RANGE
  
  ResultVariant := GL.GetParameter(Param);
  if VarIsEmpty(ResultVariant) then
    raise Exception.CreateFmt('Unexpected VarIsEmpty for glGetFloat2(%d)', [
      Param
    ]);
  if VarIsNull(ResultVariant) then
    raise Exception.CreateFmt('Unexpected VarIsNull for glGetFloat2(%d)', [
      Param
    ]);  
  ResultArray := TJSFloat32Array.Cast(ResultVariant);
  if ResultArray.Length <> 2 then
    raise Exception.CreateFmt('Expected 2 return values for glGetFloat2(%d), got %d', [
      Param,
      ResultArray.Length
    ]);
  for I := 0 to 1 do
    Result.Data[I] := ResultArray[I];
{$else}
begin
  FillChar(Result, SizeOf(Result), 0);
  glGetFloatv(Param, @Result)
  {$endif}
end;

function glGetInteger(const Param: TGLEnum): TGLint;
{$ifdef CASTLE_WEBGL}
var
  ResultVariant: Variant;
{$endif}
begin
  { Just for test, to somewhat simulate hardware with lower
    GL_MAX_TEXTURE_UNITS_ARB,
  if Param = GL_MAX_TEXTURE_UNITS_ARB then
  begin
    Result := 2;
    Exit;
  end;}

  {$ifdef CASTLE_WEBGL}
  ResultVariant := GL.GetParameter(Param);
  if VarIsEmpty(ResultVariant) then
    raise Exception.CreateFmt('Unexpected VarIsEmpty for glGetFloat(%d)', [
      Param
    ]);
  if VarIsNull(ResultVariant) then
    raise Exception.CreateFmt('Unexpected VarIsNull for glGetFloat(%d)', [
      Param
    ]);
  Result := ResultVariant;
  {$else}
  FillChar(Result, SizeOf(Result), 0);
  glGetIntegerv(Param, @Result);
  {$endif}
end;

function glGetInteger2(const Param: TGLEnum): TVector2Integer;
{$ifdef CASTLE_WEBGL}
var
  ResultVariant: Variant;
  ResultArray: IJSInt32Array;
  I: Integer;
begin
  // TODO: web: see glGetFloat2, this fails with the same error
  
  Exit(Vector2Integer(10 * 1000, 10 * 1000)); // sensible result of MAX_VIEWPORT_DIMS
  
  ResultVariant := GL.GetParameter(Param);
  ResultArray := TJSInt32Array.Cast(ResultVariant);
  if ResultArray.Length <> 2 then
    raise Exception.CreateFmt('Expected 2 return values for glGetInteger2(%d), got %d', [
      Param,
      ResultArray.Length
    ]);
  for I := 0 to 1 do
    Result.Data[I] := ResultArray[I];
{$else}
begin
  FillChar(Result, SizeOf(Result), 0);
  glGetIntegerv(Param, @Result);
{$endif}
end;

function glGetBoolean(const Param: TGLEnum): TGLboolean;
{$ifdef CASTLE_WEBGL}
var
  ResultVariant: Variant;
{$endif}
begin
  {$ifdef CASTLE_WEBGL}
  ResultVariant := GL.GetParameter(Param);
  if VarIsEmpty(ResultVariant) then
    raise Exception.CreateFmt('Unexpected VarIsEmpty for glGetFloat(%d)', [
      Param
    ]);
  if VarIsNull(ResultVariant) then
    raise Exception.CreateFmt('Unexpected VarIsNull for glGetFloat(%d)', [
      Param
    ]);
  Result := ResultVariant;
  {$else}
  FillChar(Result, SizeOf(Result), 0);
  glGetBooleanv(Param, @Result)
  {$endif}
end;

{ ---------------------------------------------------------------------------- }

{$ifndef OpenGLES}

procedure glColorv(const v: TVector3Byte);
begin
  glColor3ubv(@v);
  {$warnings off} // consciously using deprecated in deprecated
  CurrentColor := Vector4(Vector3(V), 1);
  {$warnings on}
end;

procedure glColorv(const v: TVector4Byte);
begin
  glColor4ubv(@v);
  {$warnings off} // consciously using deprecated in deprecated
  CurrentColor := Vector4(V);
  {$warnings on}
end;

procedure glColorv(const v: TVector3);
begin
  glColor3fv(@v);
  {$warnings off} // consciously using deprecated in deprecated
  CurrentColor := Vector4(V, 1);
  {$warnings on}
end;

procedure glColorv(const v: TVector4);
begin
  glColor4fv(@v);
  {$warnings off} // consciously using deprecated in deprecated
  CurrentColor := V;
  {$warnings on}
end;

procedure CastleGlClipPlane(plane: TGLenum; const V: TVector4Double);
begin
  glClipPlane(plane, @V);
end;

procedure glNormalv(const v: TVector3); begin glNormal3fv(@v); end;

procedure glMaterialv(face, Param: TGLEnum; const params: TVector4);  begin glMaterialfv(face, Param, @params); end;

procedure glVertexv(const v: TVector2);  begin glVertex2fv(@v); end;
procedure glVertexv(const v: TVector3);  begin glVertex3fv(@v); end;
procedure glVertexv(const v: TVector4);  begin glVertex4fv(@v); end;

procedure glTexCoordv(const v: TVector2);  begin glTexCoord2fv(@v); end;
procedure glTexCoordv(const v: TVector3);  begin glTexCoord3fv(@v); end;
procedure glTexCoordv(const v: TVector4);  begin glTexCoord4fv(@v); end;

procedure glTexGenv(coord, Param: TGLenum; const params: TVector4);  begin glTexGenfv(coord, Param, @params); end;

procedure glLightv(light, Param: TGLEnum; const params: TVector4);  begin glLightfv(light, Param, @params); end;
procedure glLightv(light, Param: TGLEnum; const params: TVector3);  begin glLightfv(light, Param, @params); end;

procedure glLightModelv(Param: TGLenum; const params: TVector4); overload; begin glLightModelfv(Param, @params); end;

procedure glFogv(Param: TGLEnum; const params: TVector4);  begin glFogfv(Param, @params); end;

procedure glMultMatrix(const m: TMatrix4); begin glMultMatrixf(@m) end;
procedure glLoadMatrix(const m: TMatrix4); begin glLoadMatrixf(@m) end;

procedure glTexEnvv(target, Param: TGLEnum; const params: TVector4); begin glTexEnvfv(target, Param, @params); end;

{$endif}

{ Various helpers ------------------------------------------------------------ }

procedure GLSetEnabled(Value: TGLenum; IsEnabled: boolean);
begin
  if IsEnabled then glEnable(Value) else glDisable(Value);
end;

function BoolToGL(const Value: boolean): TGLboolean;
begin
  if Value then
    Result := GL_TRUE
  else
    Result := GL_FALSE;
end;

function GLEnableTexture(const Target: TEnableTextureTarget): boolean;
begin
  Result := true;

  if GLFeatures.EnableFixedFunction then
  begin
    {$ifndef OpenGLES}

    case Target of
      etNone: begin
          glDisable(GL_TEXTURE_2D);
          if GLFeatures.TextureCubeMap then glDisable(GL_TEXTURE_CUBE_MAP);
          if GLFeatures.Texture3D then glDisable(GL_TEXTURE_3D);
        end;
      et2D: begin
          glEnable(GL_TEXTURE_2D);
          if GLFeatures.TextureCubeMap then glDisable(GL_TEXTURE_CUBE_MAP);
          if GLFeatures.Texture3D then glDisable(GL_TEXTURE_3D);
        end;
      etCubeMap: begin
          glDisable(GL_TEXTURE_2D);
          if GLFeatures.TextureCubeMap then glEnable(GL_TEXTURE_CUBE_MAP) else Result := false;
          if GLFeatures.Texture3D then glDisable(GL_TEXTURE_3D);
        end;
      et3D: begin
          glDisable(GL_TEXTURE_2D);
          if GLFeatures.TextureCubeMap then glDisable(GL_TEXTURE_CUBE_MAP);
          if GLFeatures.Texture3D then glEnable(GL_TEXTURE_3D) else Result := false;
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('GLEnableTexture:Target?');
      {$endif}
    end;

    {$endif}
  end;
end;

{$endif read_implementation}
