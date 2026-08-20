{%MainUnit castlerendererinternalshader.pas}
{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Support for geometry shaders. }

{$ifdef read_interface_in_shader_class}

{ Find and process /* VARYING-PASSTHROUGH-GEOMETRY-SHADERS */,
  following their docs in src/scene/glsl/README.md. }
procedure GeometryShaders_ProcessVaryingPassthrough;

{ Replace CASTLE_GEOMETRY_INPUT_SIZE macro in geometry shader,
  see https://castle-engine.io/x3d_implementation_shaders.php#section_geometry_input_size . }
procedure GeometryShaders_ReplaceInputSize;

{ Reorder and concatenate geometry shaders to make layout directives
  appear before any use of unsized arrays in all "compilation units".

  Move the DefaultGeometryShader code to the end, because it does:

    xxx_geoshader = xxx[index]

  and we cannot access *unsized* array xxx with non-constant index
  before we've seen the layout directives.
  Also, concatenate all geometry shader code into a single string,
  because we need layout directives to be before any use of unsized arrays.

  Without this fix, we would see errors like

    error: unsized array index must be constant

  or
    ERROR: 0:...: '[' : layout must be declared before indexing unsized varying input array with a variable

  when opening on Intel GPUs models using geometry shaders in Effect nodes,
  like

    demo-models/compositing_shaders/geometry_shader_*.x3dv
}
procedure GeometryShaders_MoveToLast;

{ Once we are certain that we have a geometry shader
  (that is, one of Source[stGeometry][...] should have main(),
  and we plan to compile Source[stGeometry][...]), do some final adjustments.

  This calls 3 methods declared above.

  If we don't have a full geometry shader, then we just clear Source[stGeometry]
  so it will not get compiled. }
procedure GeometryShaders_Finalize;

{$endif read_interface_in_shader_class}

{$ifdef read_implementation}

type
  TVaryingPassthrough = class
    { Varying name in the vertex shader, and the geometry shader input. }
    VaryingName: String;
    { Geometry shader output, and the fragment shader input. }
    VaryingNameForFragment: String;
    { GLSL type of the varying, like 'vec4'. }
    VaryingType: String;
  end;
  TVaryingPassthroughList = {$ifdef FPC}specialize{$endif} TObjectList<TVaryingPassthrough>;

procedure TShader.GeometryShaders_ProcessVaryingPassthrough;
const
  SMarker = '/* VARYING-PASSTHROUGH-GEOMETRY-SHADERS */';

  { Interpret the "varying <type> <name>;" declaration that follows the marker.
    AfterMarker is the position in ShaderCode right after the marker.
    Sets NextSearch to the position in ShaderCode right after this declaration,
    so that the caller can continue looking for the next marker. }
  function ParseVarying(const ShaderCode: String; const AfterMarker: Integer;
    out NextSearch: Integer): TVaryingPassthrough;
  var
    SemicolonPos: Integer;
    Declaration, VaryingType: String;
    Tokens: TCastleStringList;
  begin
    SemicolonPos := PosEx(';', ShaderCode, AfterMarker);
    if SemicolonPos = 0 then
      raise EGLSLError.CreateFmt('Missing ";" after "%s"', [SMarker]);
    NextSearch := SemicolonPos + 1;

    Declaration := Trim(Copy(ShaderCode, AfterMarker, SemicolonPos - AfterMarker));

    Tokens := CreateTokens(Declaration);
    try
      { Explicitly fail on anything unexpected.
        The SMarker was used deliberately, so make an exception if we cannot
        understand the declaration following it. }
      if (Tokens.Count <> 3) or
         (Tokens[0] <> 'varying') then
        raise EGLSLError.CreateFmt('Expected "varying <type> <name>;" after "%s", but got "%s"', [
          SMarker,
          Declaration
        ]);

      VaryingType := Tokens[1];
      { The generated code does "<type>(0.0)", so the type has to be
        constructible from a single float. }
      if not ArrayContainsString(VaryingType, [
        'float',
        'vec2', 'vec3', 'vec4',
        'mat2', 'mat3', 'mat4'
      ]) then
        raise EGLSLError.CreateFmt('Invalid type "%s" for varying variable marked with "%s"', [
          VaryingType,
          SMarker
        ]);

      Result := TVaryingPassthrough.Create;
      Result.VaryingName := Tokens[2];
      Result.VaryingNameForFragment := Result.VaryingName + '_geoshader';
      Result.VaryingType := VaryingType;
    finally FreeAndNil(Tokens) end;
  end;

var
  VaryingPassthroughList: TVaryingPassthroughList;
  Varying: TVaryingPassthrough;
  I, MarkerPos, SearchFrom: Integer;
  ShaderCode: String;
  GeometryVertexDeclare, GeometryVertexSet,
    GeometryVertexZero, GeometryVertexAdd, FragmentRename: String;
begin
  { We need our default geometry_shader_utils.gs as Source[stGeometry][0]
    for this process, since at the end we will plug code into
    '/* PLUG...' places in Source[stGeometry][0]. }
  if Source[stGeometry].Count = 0 then
    Exit;

  VaryingPassthroughList := TVaryingPassthroughList.Create(true);
  try
    { Scan all vertex shaders to get varying variables marked with SMarker.
      Each found marker creates a new TVaryingPassthrough,
      which we then use to generate code in geometry and fragment shaders. }
    for I := 0 to Source[stVertex].Count - 1 do
    begin
      ShaderCode := Source[stVertex][I];
      SearchFrom := 1;
      repeat
        MarkerPos := PosEx(SMarker, ShaderCode, SearchFrom);
        if MarkerPos = 0 then
          Break;
        VaryingPassthroughList.Add(ParseVarying(
          ShaderCode, MarkerPos + Length(SMarker), SearchFrom));
      until false;
    end;

    GeometryVertexDeclare := '';
    GeometryVertexSet := '';
    GeometryVertexZero := '';
    GeometryVertexAdd := '';
    FragmentRename := '';

    for Varying in VaryingPassthroughList do
    begin
      GeometryVertexDeclare := GeometryVertexDeclare + Format(
        'in %s %s[CASTLE_GEOMETRY_INPUT_SIZE];' + NL +
        'out %s %s;' + NL, [
        Varying.VaryingType, Varying.VaryingName,
        Varying.VaryingType, Varying.VaryingNameForFragment
      ]);
      GeometryVertexSet  := GeometryVertexSet + Format(
        '%s  = %s[index];' + NL, [
        Varying.VaryingNameForFragment, Varying.VaryingName
      ]);
      GeometryVertexZero := GeometryVertexZero + Format(
        '%s = %s(0.0);' + NL, [
        Varying.VaryingNameForFragment, Varying.VaryingType
      ]);
      { Note: Nvidia may warn here "... might be used before being initialized".
        Which is of course true --- but we depend that author will always call
        geometryVertexZero() before geometryVertexAdd(). }
      GeometryVertexAdd  := GeometryVertexAdd + Format(
        '%s += %s[index] * scale;' + NL, [
        Varying.VaryingNameForFragment, Varying.VaryingName
      ]);
      { Use "#define" to rename the varying variables in the fragment shader.
        The GLSL preprocessor replaces whole tokens, so we don't need to
        implement logic like "search and replace only words, omitting GLSL comments". }
      FragmentRename := FragmentRename + Format(
        '#define %s %s' + NL, [
        Varying.VaryingName, Varying.VaryingNameForFragment
      ]);
    end;

    if GeometryVertexDeclare <> '' then
    begin
      PlugDirectly(Source[stGeometry], 0, '/* PLUG-DECLARATIONS'         , GeometryVertexDeclare, false);
      PlugDirectly(Source[stGeometry], 0, '/* PLUG: geometry_vertex_set' , GeometryVertexSet    , false);
      PlugDirectly(Source[stGeometry], 0, '/* PLUG: geometry_vertex_zero', GeometryVertexZero   , false);
      PlugDirectly(Source[stGeometry], 0, '/* PLUG: geometry_vertex_add' , GeometryVertexAdd    , false);

      { Make the fragment shader read the geometry shader output.

        Note that we insert this *early*, before the "varying ..." declaration
        in fragment shader, which must have rename too (must declare variable
        with _geoshader suffix). Otherwise, when using geometry shader with
        textures, we will get errors

          The XXX shader uses varying castle_TexCoord0, but previous shader does not write to it.

        ... since we failed to use "castle_TexCoord0_geoshader" (with
        the "_geoshader" suffix) in fragment shader.

        ( Careful: NVidia drivers don't complain about this,
        and Intel (Mesa) drivers on Linux don't complain about this.
        Only Intel drivers on Windows complain about this explicitly,
        confirmed on "Intel(R) UHD Graphics". )

        Testcase: Open any of demo-models/compositing_shaders/geometry_shader_*.x3dv
        on Windows, with Intel GPU. }
      {$ifdef OpenGLES}
      { Do not add it to all Source[stFragment], as then GLSL compiler
        will say "macro redefinition", because we glue all parts for OpenGLES. }
      if Source[stFragment].Count > 0 then
        PlugDirectly(Source[stFragment], 0, '/* PLUG-DECLARATIONS-EARLY */', FragmentRename, true);
      {$else}
      for I := 0 to Source[stFragment].Count - 1 do
        PlugDirectly(Source[stFragment], I, '/* PLUG-DECLARATIONS-EARLY */', FragmentRename, true);
      {$endif}
    end;
  finally FreeAndNil(VaryingPassthroughList) end;
end;

procedure TShader.GeometryShaders_ReplaceInputSize;
var
  I: Integer;
  GeometryInputSize: String;
begin
  if GLVersion.VendorType <> gvNvidia then
    GeometryInputSize := 'gl_in.length()'
  else
    GeometryInputSize := '';

  { Replace CASTLE_GEOMETRY_INPUT_SIZE }
  for I := 0 to Source[stGeometry].Count - 1 do
    Source[stGeometry][I] := StringReplace(Source[stGeometry][I],
      'CASTLE_GEOMETRY_INPUT_SIZE', GeometryInputSize, [rfReplaceAll]);
end;

procedure TShader.GeometryShaders_MoveToLast;
var
  I: Integer;
  CompleteGeometryShader: String;
  FoundShaderMoveToLast: Boolean;
  ShaderMoveToLastContents: String;
begin
  FoundShaderMoveToLast := false;
  for I := 0 to Source[stGeometry].Count - 1 do
    if Pos('/* SHADER-MOVE-TO-LAST */', Source[stGeometry][I]) <> 0 then
    begin
      ShaderMoveToLastContents := Source[stGeometry][I];
      Source[stGeometry].Delete(I);
      Source[stGeometry].Add(ShaderMoveToLastContents);
      FoundShaderMoveToLast := true;
      Break;
    end;
  if FoundShaderMoveToLast then
  begin
    CompleteGeometryShader := GlueStrings(Source[stGeometry], NL);
    Source[stGeometry].Clear;
    Source[stGeometry].Add(CompleteGeometryShader);
  end;
end;

procedure TShader.GeometryShaders_Finalize;
begin
  if HasGeometryMain then
  begin
    GeometryShaders_ProcessVaryingPassthrough;
    GeometryShaders_ReplaceInputSize;
    GeometryShaders_MoveToLast;
  end else
    Source[stGeometry].Clear;
end;

{$endif read_implementation}