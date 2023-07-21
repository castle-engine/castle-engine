{%MainUnit castleinternalrenderer.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Mesh rendering.
  This is internal for CastleInternalRenderer unit and
  used by TRenderer.

  - This was split from TRenderer, first of all to make
    TRenderer smaller.
    TRenderer class is already large and complicated.

  - TMeshRenderer and descendants are created
    only for the time of one RenderShape call.
    The allows to reason about them easier.

  - Note: Avoid abusing the fact that this is in the same unit
    as TRenderer, avoid accessing TRenderer private stuff.
    The rendering classes are already complicated,
    keeping them cleanly separated makes it easier to reason about them.
}

type
  { Base abstract class for rendering using VBO, shaders --- the normal
    OpenGL stuff. }
  TMeshRenderer = class
  private
    FRenderer: TRenderer;
    FShape: TX3DRendererShape;
    FGeometry: TAbstractGeometryNode;
    FState: TX3DGraphTraverseState;
  protected
    { Current rendering properties, constant for the whole
      lifetime of the renderer, set in constructor. }
    property Renderer: TRenderer read FRenderer;
    property Shape: TX3DRendererShape read FShape;
    property Geometry: TAbstractGeometryNode read FGeometry;
    property State: TX3DGraphTraverseState read FState;
  public
    PrepareRenderShape: Boolean;

    CurrentMeshShader: TX3DShaderProgramBase;

    { Complete transformation from shape local coordinates to camera shape:
      Camera transformation * scene transformation * shape transformation. }
    ShapeModelView: TMatrix4;

    RenderOptions: TCastleRenderOptions;

    { Custom shader to use, if non-nil:
      Overrides whatever shaders have been determined
      for the rendering (built-in CGE or from X3D file).
      In practice, used now to set shaders when generating shadow maps. }
    CustomShaders: TCustomShaders;

    constructor Create(ARenderer: TRenderer; AShape: TX3DRendererShape);

    { Render the whole geometry.

      For some details what is, and what is not yet set in OpenGL
      before this, you have to see TVRMLOpenGLRender.RenderShape
      implementation.

      Material for VRML 1.0: currently bound material is zero.
      You can set Arrays.Color inside for VRML 1.0 material changes inside.

      Texture: you only have to generate texture coordinates in descendants.
      For all texture units < TexCoordsNeeded. (For other tex units,
      do not generate texture coords,
      they may not be even declared in shader code.)
      Do not change here the bound texture or operate on texture matrix
      or texture enabled state. }
    procedure Render; virtual; abstract;
  end;

  { Base abstract class for rendering nodes with explicit "coord" field.
    (Both IndexedXxx and not indexed). Uses TArraysGenerator
    instance to generate TGeometryArrays contents, then renders them. }
  TBaseCoordinateRenderer = class(TMeshRenderer)
  private
    { Available inside RenderCoordinate* }
    Arrays: TGeometryArrays;
    function PerVertexColor: Boolean;
  protected
    { Override these to render contents of Arrays.
      These are all called only when Coord is assigned.
      RenderCoordinateBegin, RenderCoordinateEnd are called around actual
      vertex arrays rendering.
      @groupBegin }
    procedure RenderCoordinateBegin; virtual;
    procedure RenderCoordinateEnd; virtual;
    { @groupEnd }
  public
    { Set this to non-zero to use this VBO during rendering,
      assuming Arrays contents are already loaded there.
      In this case, Arrays data must be already freed
      (by TGeometryArrays.FreeData). }
    Vbo: TVboArrays;

    Vao: TVertexArrayObject;

    { You have to set this before calling Render, to enable proper shading. }
    Shader: TShader;

    { Used (and updated) to the number of OpenGL texture units reserved. }
    BoundTextureUnits: Cardinal;

    Lighting: boolean;

    { Don't override this, it is already overridden here
      to do everything necessary. }
    procedure Render; override;
  end;

  TTextureCoordinateRenderer = class(TBaseCoordinateRenderer)
  {$ifndef OpenGLES}
  private
    RestoreDefaultTexCoord: boolean;
  {$endif}
  protected
    procedure RenderCoordinateBegin; override;
    procedure RenderCoordinateEnd; override;
  end;

  TFogCoordinateRenderer = class(TTextureCoordinateRenderer)
  protected
    procedure RenderCoordinateBegin; override;
    procedure RenderCoordinateEnd; override;
  end;

  TColorPerVertexCoordinateRenderer = class(TFogCoordinateRenderer)
  {$ifndef OpenGLES}
  private
    MaterialFromColorEnabled: boolean;
  {$endif}
  protected
    procedure RenderCoordinateBegin; override;
    procedure RenderCoordinateEnd; override;
  end;

  TShaderCoordinateRenderer = class(TColorPerVertexCoordinateRenderer)
  protected
    procedure RenderCoordinateBegin; override;
  end;

  TCompleteCoordinateRenderer = TShaderCoordinateRenderer;

{ TMeshRenderer ---------------------------------------------------------- }

constructor TMeshRenderer.Create(ARenderer: TRenderer;
  AShape: TX3DRendererShape);
begin
  inherited Create;

  FRenderer := ARenderer;
  FShape := AShape;
  FGeometry := AShape.Geometry;
  FState := AShape.State;
end;

{ TBaseCoordinateRenderer ------------------------------------------------ }

procedure TBaseCoordinateRenderer.Render;
var
  Attribs: TGLSLAttributeList;

  procedure EnableVertexAttrib(GLSLProgram: TGLSLProgram);
  var
    I: Integer;
    A: TGLSLAttribute;
  begin
    if Attribs = nil then
      Attribs := TGLSLAttributeList.Create;
    for I := 0 to Arrays.Attribs.Count - 1 do
    begin
      A := GLSLProgram.AttributeOptional(Arrays.Attribs[I].Name);
      if A.Location = -1 then
      begin
        { Do not make warnings about missing internal attributes
          (for the same reason as uniform values, see GLRendererShader). }
        if not Arrays.Attribs[I].Internal then
          WritelnWarning('VRML/X3D', 'Shader attribute "' + Arrays.Attribs[I].Name + '" not found (or not used)');
        Continue;
      end;
      case Arrays.Attribs[I].AType of
        atFloat  : A.EnableArraySingle (Vao, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));

        atVector2: A.EnableArrayVector2(Vao, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));
        atVector3: A.EnableArrayVector3(Vao, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));
        atVector4: A.EnableArrayVector4(Vao, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));

        atMatrix3: A.EnableArrayMatrix3(Vao, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));
        atMatrix4: A.EnableArrayMatrix4(Vao, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));

        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('Arrays.Attribs[I].AType?');
        {$endif}
      end;
      Attribs.Add(A);
    end;
  end;

  procedure DisableArrays;
  var
    I: Integer;
  begin
    if CurrentMeshShader = nil then
    begin
      {$ifndef OpenGLES}
      { disable fixed-function parameters }
      glDisableClientState(GL_VERTEX_ARRAY);
      glDisableClientState(GL_NORMAL_ARRAY);

      if RenderOptions.Mode = rmFull then
        glDisableClientState(GL_COLOR_ARRAY);

      { Note: For simplicity, we no longer support fog coordinate in fixed-function.
        We avoid extra alternative code path for GL_EXT_fog_coord or GL_version_1_4 that made it core.
      if Arrays.HasFogCoord and GLFeatures.EXT_fog_coord then
        glDisableClientState(GL_FOG_COORDINATE_ARRAY_EXT);
      }

      if GLFeatures.UseMultiTexturing then
      begin
        for I := 0 to Arrays.TexCoords.Count - 1 do
          if (Arrays.TexCoords[I] <> nil) and
             (Arrays.TexCoords[I].Generation = tgExplicit) then
          begin
            glClientActiveTexture(GL_TEXTURE0 + I);
            glDisableClientState(GL_TEXTURE_COORD_ARRAY);
          end;
      end else
      if (Arrays.TexCoords.Count <> 0) and
         (Arrays.TexCoords[0] <> nil) and
         (Arrays.TexCoords[0].Generation = tgExplicit) then
      begin
        glDisableClientState(GL_TEXTURE_COORD_ARRAY);
      end;
      {$endif}
    end;

    if Attribs <> nil then
    begin
      for I := 0 to Attribs.Count - 1 do
        Attribs[I].DisableArray;
      FreeAndNil(Attribs);
    end;
  end;

  { Call one of MaterialXxx, to pass shader uniforms suitable
    for the current lighting model. }
  procedure FinalizeLightingModel;

    { Pass shader uniforms for Phong calculation. }
    procedure MaterialPhong(const M: TPhongMaterialInfo);
    begin
      Assert(Shader.LightingModel = lmPhong); // should be set by InitializeLightingModel

      CurrentMeshShader.UniformCastle_MaterialDiffuseAlpha.SetValue(Vector4(M.DiffuseColor, M.Opacity));
      CurrentMeshShader.UniformCastle_MaterialShininess.SetValue(M.ShininessExp);
      CurrentMeshShader.UniformCastle_MaterialAmbient.SetValue(M.AmbientColor);
      CurrentMeshShader.UniformCastle_MaterialSpecular.SetValue(M.SpecularColor);
      CurrentMeshShader.UniformCastle_MaterialEmissive.SetValue(M.EmissiveColor);
      CurrentMeshShader.UniformCastle_GlobalAmbient.SetValue(RenderContext.GlobalAmbient);

      { castle_UnlitColor is used when we calculated Lighting = false
        (in castlerenderer_materials.inc) and material is TPhongMaterialInfo.
        This means that TPhongMaterialInfo has PureEmissive.
        (Note: the case when Lighting = false because of
        RenderOptions.Lighting = false was covered earlier,
        instead of calling MaterialPhongInitialize). }
      CurrentMeshShader.UniformCastle_UnlitColor.SetValue(Vector4(M.EmissiveColor, M.Opacity));
    end;

    { Pass shader uniforms for Physical (PBR) lighting calculation. }
    procedure MaterialPhysical(const M: TPhysicalMaterialInfo);
    begin
      Assert(Shader.LightingModel = lmPhysical); // should be set by InitializeLightingModel
      CurrentMeshShader.UniformCastle_MaterialBaseAlpha.SetValue(Vector4(M.BaseColor, M.Opacity));
      CurrentMeshShader.UniformCastle_MaterialEmissive.SetValue(M.EmissiveColor);
      CurrentMeshShader.UniformCastle_MaterialMetallic.SetValue(M.Metallic);
      CurrentMeshShader.UniformCastle_MaterialRoughness.SetValue(M.Roughness);
    end;

    { Pass shader uniforms for unlit lighting calculation. }
    procedure MaterialUnlit(const M: TUnlitMaterialInfo);
    begin
      Assert(Shader.LightingModel = lmUnlit); // should be set by InitializeLightingModel
      CurrentMeshShader.UniformCastle_MaterialEmissiveAlpha.SetValue(Vector4(M.EmissiveColor, M.Opacity));
    end;

    { Pass shader uniforms for unlit (with explicit color) lighting calculation. }
    procedure MaterialUnlitExplicit(const Color: TCastleColor);
    begin
      Assert(Shader.LightingModel = lmUnlit); // should be set by InitializeLightingModel
      CurrentMeshShader.UniformCastle_MaterialEmissiveAlpha.SetValue(Color);
    end;

  begin
    if CustomShaders <> nil then
      // right now CustomShaders <> nil means "rendering to shadow map", so no need for any colors
      MaterialUnlitExplicit(White)
    else
    if RenderOptions.Mode = rmSolidColor then
      { In case of rmSolidColor, force using unlit with RenderOptions.SolidColor.
        Note that castlerenderer_materials.inc made sure that Lightning = false
        in case of rmSolidColor, so we know it is rendered with LIT undefined. }
      MaterialUnlitExplicit(Vector4(RenderOptions.SolidColor, 1))
    else
    if (not RenderOptions.Lighting) and (Shader.Material <> nil) then
      { In case we have a material, but user requested RenderOptions.Lighting = false,
        use MainColor. Otherwise we would e.g. use EmissiveColor in case of Phong,
        which would usually be black.
        RenderOptions.Lighting = false is a debug rendering anyway, not covered
        by X3D spec equations. }
      MaterialUnlitExplicit(Vector4(Shader.Material.MainColor, Shader.Material.Opacity))
    else
    if Arrays.ForceUnlit then
      MaterialUnlitExplicit(Arrays.ForcedUnlitColor)
    else
    if Shader.Material is TPhongMaterialInfo then
      MaterialPhong(TPhongMaterialInfo(Shader.Material))
    else
    if Shader.Material is TPhysicalMaterialInfo then
      MaterialPhysical(TPhysicalMaterialInfo(Shader.Material))
    else
    if Shader.Material is TUnlitMaterialInfo then
      MaterialUnlit(TUnlitMaterialInfo(Shader.Material))
    else
      { Unlit white is used when material = nil. }
      MaterialUnlitExplicit(White);
  end;

const
  ToMode: array [TGeometryPrimitive] of TGLenum = (
    GL_TRIANGLES,
    GL_TRIANGLE_FAN,
    GL_TRIANGLE_STRIP,
    GL_LINE_STRIP,
    GL_LINE_LOOP,
    GL_LINES,
    GL_POINTS
  );
var
  I: Integer;
  First: Cardinal;
  Mode: TGLenum;
  TexCoordAttrib: TGLSLAttribute;
begin
  Attribs := nil;

  RenderCoordinateBegin;
  try
    if not PrepareRenderShape then
    begin
      { Initialize vertex arrays that we we will use with indexed nodes. }
      if Vbo[vtCoordinate] <> 0 then
        glBindBuffer(GL_ARRAY_BUFFER, Vbo[vtCoordinate]);

      {$ifdef OpenGLES}
      Assert(CurrentMeshShader <> nil, 'OpenGL ES 2.0 rendering must go through shaders');
      {$else}
      if not GLFeatures.EnableFixedFunction then
        Assert(CurrentMeshShader <> nil, 'All rendering must go through shaders when GLFeatures.EnableFixedFunction = false');
      {$endif}

      if CurrentMeshShader <> nil then
      begin
        CurrentMeshShader.UniformCastle_ModelViewMatrix.SetValue(ShapeModelView);
        CurrentMeshShader.UniformCastle_ProjectionMatrix.SetValue(RenderContext.ProjectionMatrix);
        if Shader.NeedsNormals then
          CurrentMeshShader.UniformCastle_NormalMatrix.SetValue(ModelViewToNormalMatrix(ShapeModelView));

        FinalizeLightingModel;

        Attribs := TGLSLAttributeList.Create;

        CurrentMeshShader.AttributeCastle_Vertex.EnableArrayVector3(Vao, Arrays.CoordinateSize, PtrUInt(Arrays.Position));
        Attribs.Add(CurrentMeshShader.AttributeCastle_Vertex);

        if Shader.NeedsNormals then
        begin
          CurrentMeshShader.AttributeCastle_Normal.EnableArrayVector3(Vao, Arrays.CoordinateSize, PtrUInt(Arrays.Normal));
          Attribs.Add(CurrentMeshShader.AttributeCastle_Normal);
        end;

        if Arrays.HasTangent then
        begin
          CurrentMeshShader.AttributeCastle_Tangent.EnableArrayVector3(Vao, Arrays.CoordinateSize, PtrUInt(Arrays.Tangent));
          Attribs.Add(CurrentMeshShader.AttributeCastle_Tangent);
        end;
      end else
      begin
        { set fixed-function parameters }
        {$ifndef OpenGLES}
        glEnableClientState(GL_VERTEX_ARRAY);
        glVertexPointer(3, GL_FLOAT, Arrays.CoordinateSize, Arrays.Position);

        if Shader.NeedsNormals then
        begin
          glEnableClientState(GL_NORMAL_ARRAY);
          glNormalPointer(GL_FLOAT, Arrays.CoordinateSize, Arrays.Normal);
        end;
        {$endif not OpenGLES}
      end;

      if Vbo[vtAttribute] <> 0 then
        glBindBuffer(GL_ARRAY_BUFFER, Vbo[vtAttribute]);

      if CurrentMeshShader <> nil then
      begin
        if PerVertexColor then
        begin
          Assert(Arrays.ColorType <> ctNone);
          case Arrays.ColorType of
            ctRgb     : CurrentMeshShader.AttributeCastle_ColorPerVertex.EnableArrayVector3(Vao, Arrays.AttributeSize, PtrUInt(Arrays.ColorRgb));
            ctRgbAlpha: CurrentMeshShader.AttributeCastle_ColorPerVertex.EnableArrayVector4(Vao, Arrays.AttributeSize, PtrUInt(Arrays.ColorRgbAlpha));
            else ;
          end;
          Attribs.Add(CurrentMeshShader.AttributeCastle_ColorPerVertex);
        end;

        if Arrays.HasFogCoord then
        begin
          CurrentMeshShader.AttributeCastle_FogCoord.EnableArraySingle(Vao, Arrays.AttributeSize, PtrUInt(Arrays.FogCoord));
          Attribs.Add(CurrentMeshShader.AttributeCastle_FogCoord);
        end;

        for I := 0 to Arrays.TexCoords.Count - 1 do
          if (Arrays.TexCoords[I] <> nil) and
             (Arrays.TexCoords[I].Generation = tgExplicit) then
          begin
            // TODO: would be faster to not query GLSLProgram.Attribute every frame,
            // but save it in GLSLProgram.
            TexCoordAttrib := CurrentMeshShader.AttributeOptional('castle_MultiTexCoord' + IntToStr(I));
            if TexCoordAttrib.Location = -1 then Continue;
            TexCoordAttrib.EnableArray(Vao, 0, Arrays.TexCoords[I].Dimensions, GL_FLOAT, GL_FALSE,
              Arrays.AttributeSize, PtrUInt(Arrays.TexCoord(I, 0)));
            Attribs.Add(TexCoordAttrib);
          end;
      end else
      begin
        { set fixed-function parameters }
        {$ifndef OpenGLES}
        if PerVertexColor then
        begin
          Assert(Arrays.ColorType <> ctNone);
          glEnableClientState(GL_COLOR_ARRAY);
          case Arrays.ColorType of
            ctRgb     : glColorPointer(3, GL_FLOAT, Arrays.AttributeSize, Arrays.ColorRgb);
            ctRgbAlpha: glColorPointer(4, GL_FLOAT, Arrays.AttributeSize, Arrays.ColorRgbAlpha);
            else ;
          end;
        end else
        if (RenderOptions.Mode = rmFull) and Arrays.ForceUnlit then
          glColorv(Arrays.ForcedUnlitColor);

        { Note: For simplicity, we no longer support fog coordinate in fixed-function.
          We avoid extra alternative code path for GL_EXT_fog_coord or GL_version_1_4 that made it core.
        if Arrays.HasFogCoord and GLFeatures.EXT_fog_coord then
        begin
          glEnableClientState(GL_FOG_COORDINATE_ARRAY_EXT);
          glFogCoordPointerEXT(GL_FLOAT, Arrays.AttributeSize, Arrays.FogCoord);
        end;
        }

        if GLFeatures.UseMultiTexturing then
        begin
          for I := 0 to Arrays.TexCoords.Count - 1 do
            if (Arrays.TexCoords[I] <> nil) and
               (Arrays.TexCoords[I].Generation = tgExplicit) then
            begin
              glClientActiveTexture(GL_TEXTURE0 + I);
              glEnableClientState(GL_TEXTURE_COORD_ARRAY);
              glTexCoordPointer(Arrays.TexCoords[I].Dimensions, GL_FLOAT,
                Arrays.AttributeSize, Arrays.TexCoord(I, 0));
            end;
        end else
        if (Arrays.TexCoords.Count <> 0) and
           (Arrays.TexCoords[0] <> nil) and
           (Arrays.TexCoords[0].Generation = tgExplicit) then
        begin
          glEnableClientState(GL_TEXTURE_COORD_ARRAY);
          glTexCoordPointer(Arrays.TexCoords[0].Dimensions, GL_FLOAT,
            Arrays.AttributeSize, Arrays.TexCoord(0, 0));
        end;
        {$endif not OpenGLES}
      end;

      { We know now that CurrentMeshShader is the program set by
        TShaderCoordinateRenderer.RenderCoordinateBegin }
      if (Arrays.Attribs.Count <> 0) and (CurrentMeshShader <> nil) then
        EnableVertexAttrib(CurrentMeshShader);

      Mode := ToMode[Arrays.Primitive];
      First := 0;
      if Arrays.HasIndexes then
      begin
        if Vbo[vtIndex] <> 0 then
        begin
          RenderContext.CurrentVao := Vao;
          glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Vbo[vtIndex]);
        end;

        if Arrays.Counts = nil then
          glDrawElements(Mode, Arrays.IndexesCount,
            GLIndexConst, Pointer(Arrays.IndexesPtr(First)))
        else
          for I := 0 to Arrays.Counts.Count - 1 do
          begin
            glDrawElements(Mode, Arrays.Counts[I],
              GLIndexConst, Pointer(Arrays.IndexesPtr(First)));
            First := First + Arrays.Counts[I];
          end;
      end else
      begin
        if Arrays.Counts = nil then
          glDrawArrays(Mode, First, Arrays.Count)
        else
          for I := 0 to Arrays.Counts.Count - 1 do
          begin
            glDrawArrays(Mode, First, Arrays.Counts[I]);
            First := First + Arrays.Counts[I];
          end;
      end;

      { update Renderer.Statistics.DrawCalls }
      if Arrays.Counts = nil then
        Inc(Renderer.Statistics^.DrawCalls)
      else
        Inc(Renderer.Statistics^.DrawCalls, Arrays.Counts.Count);

      { We disable arrays explicitly. We could alternatively try
        glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT), but
        1. it makes Mesa 7.2 segfault
        2. I don't know if it really works for GLSL vertex attribs arrays.
        3. It is not available on OpenGLES. }
      DisableArrays;
    end;
  finally RenderCoordinateEnd end;
end;

procedure TBaseCoordinateRenderer.RenderCoordinateBegin;
begin
  RenderContext.CullFace := Arrays.CullFace;
  { Following glTF on what to do with negative scale transformations:

    """
    If the determinant of the transform is a negative value,
    the winding order of the mesh triangle faces should be reversed.
    This supports negative scales for mirroring geometry.
    """

    This makes backface culling remove proper faces when negative
    scale is used.
    See https://github.com/castle-engine/castle-engine/issues/336 . }
  if RenderOptions.RobustNegativeScale and
     (ShapeModelView.Determinant < 0) then
    RenderContext.FrontFaceCcw := not Arrays.FrontFaceCcw
  else
    RenderContext.FrontFaceCcw := Arrays.FrontFaceCcw;
end;

procedure TBaseCoordinateRenderer.RenderCoordinateEnd;
begin
end;

function TBaseCoordinateRenderer.PerVertexColor: Boolean;
begin
  Result := (RenderOptions.Mode = rmFull) and (Arrays.ColorType <> ctNone);
end;

{ TTextureCoordinateRenderer ----------------------------------------- }

{$ifndef OpenGLES}
{ If multitexturing available, sets fixed-function texture coordinate for texture
  unit TextureUnit (by appropriate glMultiTexCoord).
  Otherwise (when no multitexturing), sets texture coordinate for
  the only texture unit (glTexCoord).

  The only thing that you have to care about is to specify TextureUnit <
  GLFeatures.MaxTextureUnits. Everything else (whether multitexturing
  available, and shifting TextureUnit by GL_TEXTURE0) is taken care of inside here. }
procedure MultiTexCoord(const TextureUnit: Cardinal; const TexCoord: TVector4);
begin
  if GLFeatures.UseMultiTexturing then
    glMultiTexCoord4fv(GL_TEXTURE0 + TextureUnit, @TexCoord)
  else
    glTexCoord4fv(@TexCoord);
end;
{$endif}

procedure TTextureCoordinateRenderer.RenderCoordinateBegin;

  { Enable and set parameters of texture generation,
    for texture units where TexCoords[].Generation[] <> tgExplicit. }
  procedure EnableTextureGen(const TexUnit: Cardinal;
    TexCoord: TGeometryTexCoord);

    {$ifndef OpenGLES}
    { Do the necessary things for WorldSpaceNormal and
      WorldSpaceReflectionVector in fixed-function pipeline.
      Note that the shader pipeline counterpart
      of this is entirely inside EnableTexGen. }
    procedure TextureTransformToWorldSpace(const SuccessName, FailName: string);
    begin
      if not GLFeatures.EnableFixedFunction then
        Exit;

      { Set the 4th texture coord component (Q) to 0.0.
        Reason: We use matrix 4x4, so it will transform whole texture
        coordinate (4-valued vector, since we're in homogeneous
        coordinates). We want the generated STR to be transformed
        like a direction, so we want to set Q = always 0.0
        (in homogeneous coords, this means that vector represents
        a direction and will be accordingly transformed). }
      MultiTexCoord(TexUnit, TVector4.Zero);

      { Remember to set default tex coord 4th component back to 1
        at the end. Otherwise rendering normal textures (not cube maps)
        could be incorrect --- as the 4th texture component is 0,
        GPU doesn't know how to sample 2D or 3D texture data.
        See Barna29.x3dv from Victor Amat bugreport.

        Note that we don't remember that we need to reset it only
        on TexUnit. So RenderCoordinateEnd will just have to reset
        it on all units possibly touched by EnableTextureGen.
        It's a little unoptimal. OTOH, setting up an array to remember
        a list of TexUnit in this case would be an overkill, as in 99% cases
        you only have 1 or 2 texture units where you generate tex coords. }
      RestoreDefaultTexCoord := true;

      glMatrixMode(GL_TEXTURE);
      Renderer.PushTextureUnit(TexUnit);
      { CameraMatrix transforms from world space to camera space,
        we want to transform directions from camera space to world
        space below. So inverted CameraMatrix is exactly what we need. }
      Renderer.RenderingCamera.InverseMatrixNeeded;
      glMultMatrix(Renderer.RenderingCamera.InverseMatrix);
      glMatrixMode(GL_MODELVIEW);

      (*
      Later: remove this log, it spams a lot on some scenes,
      and possibly is incorrect on newer Mesa versions.

      if GLVersion.Mesa and Log then
        { Mesa 7.0.4 makes textures strange (like each cube face was
          1-pixel wide?, although I checked --- they are loaded with
          correct sizes). Output this on log, since it's done by renderer,
          potentially many times for a frame.
          Nothing more intelligent now than warning about this is done. }
        WritelnWarning('TexCoord', Format('Transforming tex coords to WORLDSPACE* under Mesa3D may mess up static cube maps (ImageCubeMapTexture and ComposedCubeMapTexture, but not GeneratedCubeMapTexure)',
          [SuccessName, FailName]));
      *)
    end;
    {$endif}

  const
    GenerationVectorsIdentity: TTextureGenerationVectors =
    ( (X:1; Y: 0; Z: 0; W: 0),
      (X:0; Y: 1; Z: 0; W: 0),
      (X:0; Y: 0; Z: 1; W: 0) );
    ProjectorScalingMatrix: TMatrix4 = (Data:
    ( (0.5,   0,   0, 0),
      (  0, 0.5,   0, 0),
      (  0,   0, 0.5, 0),
      (0.5, 0.5, 0.5, 1) ));
  var
    TransformToWorldSpace: boolean;
    ProjectorMatrix: TMatrix4;
  begin
    { No point in generating for more texture units.
      They are not even declared in GLSL code, and would cause GLSL errors
      if your textures use e.g. only 1 texture unit,
      but you use MultiTextureCoordinate with 2 children and 2nd children
      is TextureCoordinateGenerator.
      (testcase: x3d-tests/pbr/physical_material.x3dv) }
    if TexUnit >= Renderer.TexCoordsNeeded then
      Exit;

    TransformToWorldSpace := TexCoord.Generation in
      [tgWorldSpaceReflectionVector, tgWorldSpaceNormal];

    TRenderer.ActiveTexture(TexUnit);
    case TexCoord.Generation of
      tgSphereMap:
        Shader.EnableTexGen(TexUnit, tgSphere);
      tgCoord:
        begin
          if GLFeatures.EnableFixedFunction then
          begin
            {$ifndef OpenGLES}
            glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_S, GL_OBJECT_PLANE, GenerationVectorsIdentity[0]);
            glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_T, GL_OBJECT_PLANE, GenerationVectorsIdentity[1]);
            glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_R, GL_OBJECT_PLANE, GenerationVectorsIdentity[2]);
            {$endif}
          end;
          Shader.EnableTexGen(TexUnit, tgObject, 0, GenerationVectorsIdentity[0]);
          Shader.EnableTexGen(TexUnit, tgObject, 1, GenerationVectorsIdentity[1]);
          Shader.EnableTexGen(TexUnit, tgObject, 2, GenerationVectorsIdentity[2]);
        end;
      tgCoordEye:
        begin
          if GLFeatures.EnableFixedFunction then
          begin
            {$ifndef OpenGLES}
            { Vectors given to EYE_PLANE are transformed by inverted
              modelview matrix (at the moment of glTexGen call).
              We don't want to transform them, so just temporarily setting
              modelview matrix to identity makes things work Ok. }
            glPushMatrix;
              glLoadIdentity;
              glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
              glTexGenv(GL_S, GL_EYE_PLANE, GenerationVectorsIdentity[0]);
              glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
              glTexGenv(GL_T, GL_EYE_PLANE, GenerationVectorsIdentity[1]);
              glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
              glTexGenv(GL_R, GL_EYE_PLANE, GenerationVectorsIdentity[2]);
            glPopMatrix;
            {$endif}
          end;
          { TODO: the shader version of this is quite unoptimal,
            we pass 3 uniform vec4 vectors, make 3 dot operations,
            just to simply copy vertex_eye to texture coordinate.
            Same for tgCoord. So far, this is not used often enough
            to bother with optimization. }
          Shader.EnableTexGen(TexUnit, tgEye, 0, GenerationVectorsIdentity[0]);
          Shader.EnableTexGen(TexUnit, tgEye, 1, GenerationVectorsIdentity[1]);
          Shader.EnableTexGen(TexUnit, tgEye, 2, GenerationVectorsIdentity[2]);
        end;
      tgCameraSpaceNormal,
      tgWorldSpaceNormal:
        begin
          {$ifdef OpenGLES}
          Assert(GLFeatures.TextureCubeMap); // on OpenGLES, cubemaps are always supported
          {$else}
          if not GLFeatures.TextureCubeMap then
          begin
            WritelnWarning('VRML/X3D', 'Rendering context doesn''t support cube maps, cannot generate texture coordinates with mode = "CAMERASPACENORMAL", will do "COORD" instead');

            if GLFeatures.EnableFixedFunction then
            begin
              glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
              glTexGenv(GL_S, GL_OBJECT_PLANE, GenerationVectorsIdentity[0]);
              glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
              glTexGenv(GL_T, GL_OBJECT_PLANE, GenerationVectorsIdentity[1]);
              glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
              glTexGenv(GL_R, GL_OBJECT_PLANE, GenerationVectorsIdentity[2]);
            end;

            Shader.EnableTexGen(TexUnit, tgObject, 0, GenerationVectorsIdentity[0]);
            Shader.EnableTexGen(TexUnit, tgObject, 1, GenerationVectorsIdentity[1]);
            Shader.EnableTexGen(TexUnit, tgObject, 2, GenerationVectorsIdentity[2]);
          end else
          {$endif}
          begin
            {$ifndef OpenGLES}
            if TransformToWorldSpace then
              TextureTransformToWorldSpace('WORLDSPACENORMAL', 'CAMERASPACENORMAL');
            {$endif}
            Shader.EnableTexGen(TexUnit, tgNormal, TransformToWorldSpace);
          end;
        end;
      tgCameraSpaceReflectionVector,
      tgWorldSpaceReflectionVector:
        begin
          {$ifdef OpenGLES}
          Assert(GLFeatures.TextureCubeMap); // on OpenGLES, cubemaps are always supported
          {$else}
          if not GLFeatures.TextureCubeMap then
          begin
            WritelnWarning('VRML/X3D', 'Rendering context doesn''t support cube maps, cannot generate texture coordinates with mode = "CAMERASPACEREFLECTIONVECTOR", will do "COORD" instead');

            if GLFeatures.EnableFixedFunction then
            begin
              glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
              glTexGenv(GL_S, GL_OBJECT_PLANE, GenerationVectorsIdentity[0]);
              glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
              glTexGenv(GL_T, GL_OBJECT_PLANE, GenerationVectorsIdentity[1]);
              glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
              glTexGenv(GL_R, GL_OBJECT_PLANE, GenerationVectorsIdentity[2]);
            end;

            Shader.EnableTexGen(TexUnit, tgObject, 0, GenerationVectorsIdentity[0]);
            Shader.EnableTexGen(TexUnit, tgObject, 1, GenerationVectorsIdentity[1]);
            Shader.EnableTexGen(TexUnit, tgObject, 2, GenerationVectorsIdentity[2]);
          end else
          {$endif}
          begin
            {$ifndef OpenGLES}
            if TransformToWorldSpace then
              TextureTransformToWorldSpace('WORLDSPACEREFLECTIONVECTOR', 'CAMERASPACEREFLECTIONVECTOR');
            {$endif}
            Shader.EnableTexGen(TexUnit, tgReflection, TransformToWorldSpace);
          end;
        end;
      tgProjection:
        begin
          if Assigned(TexCoord.GenerationProjectorMatrix) then
          begin
            ProjectorMatrix := ProjectorScalingMatrix * TexCoord.GenerationProjectorMatrix();

            if GLFeatures.EnableFixedFunction then
            begin
              {$ifndef OpenGLES}
              glPushMatrix;
                glLoadMatrix(Renderer.RenderingCamera.Matrix);

                glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
                glTexGenv(GL_S, GL_EYE_PLANE, ProjectorMatrix.Rows[0]);

                glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
                glTexGenv(GL_T, GL_EYE_PLANE, ProjectorMatrix.Rows[1]);

                glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
                glTexGenv(GL_R, GL_EYE_PLANE, ProjectorMatrix.Rows[2]);

                glTexGeni(GL_Q, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
                glTexGenv(GL_Q, GL_EYE_PLANE, ProjectorMatrix.Rows[3]);
              glPopMatrix;
              {$endif}
            end;

            Renderer.RenderingCamera.InverseMatrixNeeded;
            ProjectorMatrix :=  ProjectorMatrix * Renderer.RenderingCamera.InverseMatrix;

            Shader.EnableTexGen(TexUnit, tgEye, 0, ProjectorMatrix.Rows[0]);
            Shader.EnableTexGen(TexUnit, tgEye, 1, ProjectorMatrix.Rows[1]);
            Shader.EnableTexGen(TexUnit, tgEye, 2, ProjectorMatrix.Rows[2]);
            Shader.EnableTexGen(TexUnit, tgEye, 3, ProjectorMatrix.Rows[3]);
          end;
        end;
      tgBounds2d:
        begin
          if GLFeatures.EnableFixedFunction then
          begin
            {$ifndef OpenGLES}
            glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_S, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[0]);
            glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_T, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[1]);
            {$endif}
          end;
          Shader.EnableTexGen(TexUnit, tgObject, 0, TexCoord.GenerationBoundsVector[0]);
          Shader.EnableTexGen(TexUnit, tgObject, 1, TexCoord.GenerationBoundsVector[1]);
        end;
      tgBounds3d:
        begin
          if GLFeatures.EnableFixedFunction then
          begin
            {$ifndef OpenGLES}
            glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_S, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[0]);
            glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_T, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[1]);
            glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_R, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[2]);
            {$endif}
          end;
          Shader.EnableTexGen(TexUnit, tgObject, 0, TexCoord.GenerationBoundsVector[0]);
          Shader.EnableTexGen(TexUnit, tgObject, 1, TexCoord.GenerationBoundsVector[1]);
          Shader.EnableTexGen(TexUnit, tgObject, 2, TexCoord.GenerationBoundsVector[2]);
        end;
      tgMirrorPlane:
        begin
          Shader.EnableTexGen(TexUnit, tgcMirrorPlane, false);
          Shader.MirrorPlaneUniforms := Shape.MirrorPlaneUniforms;
        end;
      else raise EInternalError.Create('EnableTextureGen - Gen?');
    end;
  end;

var
  I: Integer;
begin
  inherited;
  for I := 0 to Arrays.TexCoords.Count - 1 do
    if Arrays.TexCoords[I].Generation <> tgExplicit then
      EnableTextureGen(I, Arrays.TexCoords[I]);
end;

procedure TTextureCoordinateRenderer.RenderCoordinateEnd;
{$ifndef OpenGLES}
const
  DefaultTexCoord: TVector4 = (X:0; Y: 0; Z: 0; W: 1);
{$endif}
var
  I: Integer;
begin
  for I := 0 to Arrays.TexCoords.Count - 1 do
    if Arrays.TexCoords[I].Generation <> tgExplicit then
    begin
      { Disable OpenGL GL_TEXTURE_GEN_x, if it was (possibly)
        enabled by RenderCoordinateBegin. }
      Shader.DisableTexGen(I);
      if GLFeatures.EnableFixedFunction then
      begin
        {$ifndef OpenGLES}
        { DisableTexGen already set glActiveTexture }
        if RestoreDefaultTexCoord then
          MultiTexCoord(I, DefaultTexCoord);
        {$endif}
      end;
    end;

  inherited;
end;

{ TFogCoordinateRenderer ------------------------------------------------------- }

procedure TFogCoordinateRenderer.RenderCoordinateBegin;
begin
  inherited;

  if Arrays.HasFogCoord and
     Arrays.FogDirectValues then
  begin
    { We potentially override the fog already set by TRenderer.RenderShapeFog here. }
    Shader.ModifyFog(ftLinear, fcPassedCoordinate, 1);
  end;
end;

procedure TFogCoordinateRenderer.RenderCoordinateEnd;
begin
  inherited;
end;

{ TColorPerVertexCoordinateRenderer --------------------------------------- }

procedure TColorPerVertexCoordinateRenderer.RenderCoordinateBegin;
begin
  inherited;

  if PerVertexColor then
  begin
    { Note that the shader implementation of color-per-vertex is enabled regardless
      if we have lighting or not, as in fact this activates COLOR_PER_VERTEX
      feature, which works regardless if we're lit or unlit. }
    Assert(Arrays.ColorType <> ctNone); // PerVertexColor = true should guarantee this
    Shader.EnableColorPerVertex(Arrays.ColorMode, Arrays.ColorType);

    {$ifndef OpenGLES}
    { For fixed-function pipeline, GL_COLOR_MATERIAL is needed only if Lighting. }
    MaterialFromColorEnabled := GLFeatures.EnableFixedFunction and Lighting;
    if MaterialFromColorEnabled then
      { Note: glColorMaterial is already set by TRenderer.ClassRenderBegin }
      glEnable(GL_COLOR_MATERIAL);
    {$endif}
  end else
  begin
    {$ifndef OpenGLES}
    MaterialFromColorEnabled := false;
    {$endif}
  end;
end;

procedure TColorPerVertexCoordinateRenderer.RenderCoordinateEnd;
begin
  {$ifndef OpenGLES}
  if MaterialFromColorEnabled then
    glDisable(GL_COLOR_MATERIAL);
  {$endif}

  inherited;
end;

{ TShaderCoordinateRenderer -------------------------------------------------- }

procedure TShaderCoordinateRenderer.RenderCoordinateBegin;

  { Initialize Shader.LightingModel.
    Must be done before linking the shader.
    Logic must follow *exactly* FinalizeLightingModel (which must be done after
    linking the shader). }
  procedure InitializeLightingModel;
  begin
    if CustomShaders <> nil then
      Shader.LightingModel := lmUnlit
    else
    if RenderOptions.Mode = rmSolidColor then
      Shader.LightingModel := lmUnlit
    else
    if (not RenderOptions.Lighting) and (Shader.Material <> nil) then
      Shader.LightingModel := lmUnlit
    else
    if Arrays.ForceUnlit then
      Shader.LightingModel := lmUnlit
    else
    if Shader.Material is TPhongMaterialInfo then
      Shader.LightingModel := lmPhong
    else
    if Shader.Material is TPhysicalMaterialInfo then
      Shader.LightingModel := lmPhysical
    else
    if Shader.Material is TUnlitMaterialInfo then
      Shader.LightingModel := lmUnlit
    else
      Shader.LightingModel := lmUnlit
  end;

var
  CurrentX3DProgram: TX3DShaderProgram;
  Pass: TTotalRenderingPass;
begin
  inherited;

  InitializeLightingModel;

  if CustomShaders <> nil then
  begin
    CurrentMeshShader := CustomShaders.Shader[Shader.AlphaTest];
  end else
  begin
    { Finalize Shader settings, before calculating Shader.CodeHash }
    if (Shape.Node <> nil) and
       (Shape.Node.Appearance <> nil) then
      Shader.EnableAppearanceEffects(Shape.Node.Appearance.FdEffects);
    if Shape.State.Effects <> nil then
      Shader.EnableGroupEffects(Shape.State.Effects);
    (*
    { For now we decided to not implement per-scene RenderOptions.ColorSpace,
      as use-case would be low.
      In all practical cases, adjusting global ColorSpace seems like a better idea,
      as this should really apply to all your rendering. }
    if RenderOptions.ColorSpace <> csDefault then
      FinalColorSpace := RenderOptions.ColorSpace // use per-scene value
    else
      FinalColorSpace := CastleRenderOptions.ColorSpace; // use global variable
    *)
    Shader.ColorSpaceLinear :=
      (ColorSpace = csLinear) or
      ((ColorSpace = csLinearWhenPhysicalMaterial) and
       (Shader.Material is TPhysicalMaterialInfo));

    if not GLFeatures.EnableFixedFunction then
    begin
      Pass := Renderer.FPass;

      { calculate and use Shape.ProgramCache[Pass].ShaderProgram }
      if (Shape.ProgramCache[Pass] = nil) or
         (not(Shape.ProgramCache[Pass].Hash = Shader.CodeHash)) then
      begin
        if LogRenderer then
          WritelnLog('GLSL', 'Creating shaders for shape "%s" (new: %s, pass: %d)', [
            Shape.NiceName,
            BoolToStr((Shape.ProgramCache[Pass] = nil) or
                      (Shape.ProgramCache[Pass].ShaderProgram = nil), true),
            Pass
          ]);

        if Shape.ProgramCache[Pass] <> nil then
          RendererCache.Program_DecReference(Shape.ProgramCache[Pass]);

        Shape.ProgramCache[Pass] := RendererCache.Program_IncReference(
          Shader, Shape.NiceName);
      end;

      Assert(Shape.ProgramCache[Pass] <> nil);
      CurrentX3DProgram := Shape.ProgramCache[Pass].ShaderProgram;
      CurrentMeshShader := CurrentX3DProgram;
      if not GLFeatures.EnableFixedFunction then
        Assert(CurrentX3DProgram <> nil, 'Shader is not initialized, but it has to be (creation of fallback shaders failed?)');
      if CurrentX3DProgram <> nil then // may be nil on desktops
        Shader.SetDynamicUniforms(CurrentX3DProgram);
    end else
      CurrentMeshShader := nil;
  end;

  if CurrentMeshShader <> nil then
    CurrentMeshShader.SetupUniforms(BoundTextureUnits);

  RenderContext.CurrentProgram := CurrentMeshShader; // enable the CurrentMeshShader for OpenGL
end;
