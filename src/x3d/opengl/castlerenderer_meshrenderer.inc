{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ X3D rendering.
  This is internal for CastleRenderer unit.

  Some ideas for implementation:

  - CastleRenderer unit, and TGLRenderer class are already
    large and complicated. The idea of this file is to take some tasks
    from their hands. TMeshRenderer and descendants are created
    only for the time of one RenderShape call, and are used
    to render specific non-trivial X3D mesh nodes.

  - Since all classes within this file and CastleRenderer live
    in one Pascal unit, we could break it and access each other's
    private fields etc. We try to not do it. We try to have a clean
    documented interface. Experience with TGeneralIndexedRenderer
    taught me that this has to be really flexible and designed for
    long-term work, to encompass many nodes and features. So, while
    this is internal for CastleRenderer, it still must be implemented
    and documented cleanly.
}

type
  TMeshRenderer = class
  private
    FRenderer: TGLRenderer;
    FShape: TX3DRendererShape;
    FGeometry: TAbstractGeometryNode;
    FState: TX3DGraphTraverseState;
    FAttributes: TRenderingAttributes;
  protected
    { Current rendering properties, constant for the whole
      lifetime of the renderer, set in constructor. }
    property Renderer: TGLRenderer read FRenderer;
    property Shape: TX3DRendererShape read FShape;
    property Geometry: TAbstractGeometryNode read FGeometry;
    property State: TX3DGraphTraverseState read FState;
    property Attributes: TRenderingAttributes read FAttributes;
  public
    { If > 0, RenderShape will not actually render, only prepare
      per-shape resources for fast rendering (arrays and vbos). }
    PrepareRenderShape: Cardinal;

    CurrentMeshShader: TX3DShaderProgramBase;

    constructor Create(ARenderer: TGLRenderer; AShape: TX3DRendererShape);

    { Render the whole geometry.

      For some details what is, and what is not yet set in OpenGL
      before this, you have to see TVRMLOpenGLRender.RenderShape
      implementation.

      Material for VRML 1.0: currently bound material is zero.
      You can set Arrays.Color inside for VRML 1.0 material changes inside.

      Texture: you only have to generate texture coordinates in descendants.
      For all texture units < TexCoordsNeeded. (For other tex units,
      you don't have to (but you can,
      although they will be unused) generate texture coords.)
      Do not change here the bound texture or operate on texture matrix
      or texture enabled state. }
    procedure Render; virtual; abstract;
  end;

  { Does the mesh (regardless of material) define some color. }
  TMeshColor = (
    mcNone,
    { Use Arrays.Color, knowing that Arrays.HasColor. }
    mcColorPerVertex,
    { Use Arrays.DefaultColor, knowing that Arrays.HasDefaultColor. }
    mcDefaultColor
  );

  { Base abstract class for rendering nodes with explicit "coord" field.
    (Both IndexedXxx and not indexed). Uses TArraysGenerator
    instance to generate TGeometryArrays contents, then renders them. }
  TBaseCoordinateRenderer = class(TMeshRenderer)
  private
    { Available inside RenderCoordinate* }
    Arrays: TGeometryArrays;

    function MeshColor: TMeshColor;
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

constructor TMeshRenderer.Create(ARenderer: TGLRenderer;
  AShape: TX3DRendererShape);
begin
  inherited Create;

  FRenderer := ARenderer;
  FShape := AShape;
  FGeometry := AShape.Geometry;
  FState := AShape.State;
  FAttributes := Renderer.Attributes;
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
        atFloat  : A.EnableArraySingle       (Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));

        atVector2: A.EnableArrayVector2(Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));
        atVector3: A.EnableArrayVector3(Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));
        atVector4: A.EnableArrayVector4(Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));

        atMatrix3: A.EnableArrayMatrix3(Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));
        atMatrix4: A.EnableArrayMatrix4(Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));

        else raise EInternalError.Create('Arrays.Attribs[I].AType?');
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

      if Attributes.Mode = rmFull then
        glDisableClientState(GL_COLOR_ARRAY);

      if Arrays.HasFogCoord and GLFeatures.EXT_fog_coord then
        glDisableClientState(GL_FOG_COORDINATE_ARRAY_EXT);

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

const
  ToMode: array [TGeometryPrimitive] of TGLenum =
  ( GL_TRIANGLES, {$ifndef OpenGLES} GL_QUADS, {$endif}
    GL_TRIANGLE_FAN, GL_TRIANGLE_STRIP,
    GL_LINE_STRIP, GL_POINTS );
var
  I: Integer;
  First: Cardinal;
  Mode: TGLenum;
  TexCoordAttrib: TGLSLAttribute;
begin
  Attribs := nil;

  RenderCoordinateBegin;
  try
    if PrepareRenderShape = 0 then
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
        CurrentMeshShader.UniformCastle_ModelViewMatrix.SetValue(Shape.ModelView);
        CurrentMeshShader.UniformCastle_ProjectionMatrix.SetValue(RenderContext.ProjectionMatrix);
        CurrentMeshShader.UniformCastle_NormalMatrix.SetValue(ModelViewToNormalMatrix(Shape.ModelView));
        CurrentMeshShader.UniformCastle_MaterialDiffuseAlpha.SetValue(Shader.MaterialDiffuse[3]);
        CurrentMeshShader.UniformCastle_MaterialShininess.SetValue(Shader.MaterialShininessExp);
        CurrentMeshShader.UniformCastle_SceneColor.SetValue(Shader.MaterialEmission.XYZ +
          RenderContext.GlobalAmbient * Shader.MaterialAmbient.XYZ);
        CurrentMeshShader.UniformCastle_UnlitColor.SetValue(Shader.MaterialUnlit);

        Attribs := TGLSLAttributeList.Create;

        CurrentMeshShader.AttributeCastle_Vertex.EnableArrayVector3(Arrays.CoordinateSize, PtrUInt(Arrays.Position));
        Attribs.Add(CurrentMeshShader.AttributeCastle_Vertex);

        CurrentMeshShader.AttributeCastle_Normal.EnableArrayVector3(Arrays.CoordinateSize, PtrUInt(Arrays.Normal));
        Attribs.Add(CurrentMeshShader.AttributeCastle_Normal);
      end else
      begin
        { set fixed-function parameters }
        {$ifndef OpenGLES}
        glEnableClientState(GL_VERTEX_ARRAY);
        glVertexPointer(3, GL_FLOAT, Arrays.CoordinateSize, Arrays.Position);

        glEnableClientState(GL_NORMAL_ARRAY);
        glNormalPointer(GL_FLOAT, Arrays.CoordinateSize, Arrays.Normal);
        {$endif not OpenGLES}
      end;

      if Vbo[vtAttribute] <> 0 then
        glBindBuffer(GL_ARRAY_BUFFER, Vbo[vtAttribute]);

      if CurrentMeshShader <> nil then
      begin
        case MeshColor of
          mcColorPerVertex:
            begin
              Assert(Arrays.HasColor);
              CurrentMeshShader.AttributeCastle_ColorPerVertex.EnableArrayVector4(Arrays.AttributeSize, PtrUInt(Arrays.Color));
              Attribs.Add(CurrentMeshShader.AttributeCastle_ColorPerVertex);
            end;

          mcDefaultColor:
            begin
              Assert(Arrays.HasDefaultColor);
              { We apply Arrays.DefaultColor as Shader.MaterialUnlit,
                overriding previous castle_UnlitColor setting.
                Arrays.DefaultColor is in fact used with this intention
                ("override unlit color") by PointSet and LineSet generators
                in castlearraysgenerator_rendering.inc . }
              CurrentMeshShader.UniformCastle_UnlitColor.SetValue(Arrays.DefaultColor);
            end;
        end;

        if Arrays.HasFogCoord then
        begin
          CurrentMeshShader.AttributeCastle_FogCoord.EnableArraySingle(Arrays.AttributeSize, PtrUInt(Arrays.FogCoord));
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
            TexCoordAttrib.EnableArray(0, Arrays.TexCoords[I].Dimensions, GL_FLOAT, GL_FALSE,
              Arrays.AttributeSize, PtrUInt(Arrays.TexCoord(I, 0)));
            Attribs.Add(TexCoordAttrib);
          end;
      end else
      begin
        { set fixed-function parameters }
        {$ifndef OpenGLES}
        case MeshColor of
          mcColorPerVertex:
            begin
              Assert(Arrays.HasColor);
              glEnableClientState(GL_COLOR_ARRAY);
              glColorPointer(4, GL_FLOAT, Arrays.AttributeSize, Arrays.Color);
            end;
          mcDefaultColor:
            begin
              Assert(Arrays.HasDefaultColor);
              glColorv(Arrays.DefaultColor);
            end;
        end;

        if Arrays.HasFogCoord and GLFeatures.EXT_fog_coord then
        begin
          glEnableClientState(GL_FOG_COORDINATE_ARRAY_EXT);
          glFogCoordPointerEXT(GL_FLOAT, Arrays.AttributeSize, Arrays.FogCoord);
        end;

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
          glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Vbo[vtIndex]);

        if Arrays.Counts = nil then
          glDrawElements(Mode, Arrays.IndexesCount,
            {$ifdef GLIndexesShort} GL_UNSIGNED_SHORT {$else} GL_UNSIGNED_INT {$endif}, Pointer(Arrays.IndexesPtr(First))) else
          for I := 0 to Arrays.Counts.Count - 1 do
          begin
            glDrawElements(Mode, Arrays.Counts[I],
              {$ifdef GLIndexesShort} GL_UNSIGNED_SHORT {$else} GL_UNSIGNED_INT {$endif}, Pointer(Arrays.IndexesPtr(First)));
            First := First + Arrays.Counts[I];
          end;
      end else
      begin
        if Arrays.Counts = nil then
          glDrawArrays(Mode, First, Arrays.Count) else
          for I := 0 to Arrays.Counts.Count - 1 do
          begin
            glDrawArrays(Mode, First, Arrays.Counts[I]);
            First := First + Arrays.Counts[I];
          end;
      end;

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
  RenderContext.FrontFaceCcw := Arrays.FrontFaceCcw;
  if Renderer.Attributes.Mode = rmFull then
    Renderer.SmoothShading := not Arrays.ForceFlatShading;
end;

procedure TBaseCoordinateRenderer.RenderCoordinateEnd;
begin
end;

function TBaseCoordinateRenderer.MeshColor: TMeshColor;
begin
  Result := mcNone;
  if Attributes.Mode = rmFull then
  begin
    if Arrays.HasColor then
      Result := mcColorPerVertex else
    if Arrays.HasDefaultColor then
      Result := mcDefaultColor;
  end;
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
procedure MultiTexCoord(const TextureUnit: Cardinal; const TexCoord: TVector4f);
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
      RenderingCamera.InverseMatrixNeeded;
      glMultMatrix(RenderingCamera.InverseMatrix);
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
        WritelnLog('TexCoord', Format('Warning: Transforming tex coords to WORLDSPACE* under Mesa3D may mess up static cube maps (ImageCubeMapTexture and ComposedCubeMapTexture, but not GeneratedCubeMapTexure)',
          [SuccessName, FailName]));
      *)
    end;
    {$endif}

  const
    GenerationVectorsIdentity: TTextureGenerationVectors =
    ( (Data: (1, 0, 0, 0)),
      (Data: (0, 1, 0, 0)),
      (Data: (0, 0, 1, 0)) );
    ProjectorScalingMatrix: TMatrix4 = (Data:
    ( (0.5,   0,   0, 0),
      (  0, 0.5,   0, 0),
      (  0,   0, 0.5, 0),
      (0.5, 0.5, 0.5, 1) ));
  var
    TransformToWorldSpace: boolean;
    ProjectorMatrix: TMatrix4;
  begin
    TransformToWorldSpace := TexCoord.Generation in
      [tgWorldSpaceReflectionVector, tgWorldSpaceNormal];

    Renderer.ActiveTexture(TexUnit);
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
          Assert(GLFeatures.TextureCubeMap <> gsNone); // on OpenGLES, cubemaps are always supported
          {$else}
          if GLFeatures.TextureCubeMap = gsNone then
          begin
            WritelnWarning('VRML/X3D', 'ARB_texture_cube_map not supported by your OpenGL implementation, cannot generate texture coordinates with mode = "CAMERASPACENORMAL", will do "COORD" instead');

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
          Assert(GLFeatures.TextureCubeMap <> gsNone); // on OpenGLES, cubemaps are always supported
          {$else}
          if GLFeatures.TextureCubeMap = gsNone then
          begin
            WritelnWarning('VRML/X3D', 'ARB_texture_cube_map not supported by your OpenGL implementation, cannot generate texture coordinates with mode = "CAMERASPACEREFLECTIONVECTOR", will do "COORD" instead');

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
                glLoadMatrix(RenderingCamera.Matrix);

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

            RenderingCamera.InverseMatrixNeeded;
            ProjectorMatrix :=  ProjectorMatrix * RenderingCamera.InverseMatrix;

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
  DefaultTexCoord: TVector4 = (Data: (0, 0, 0, 1));
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
    {$ifndef OpenGLES}
    if GLFeatures.EnableFixedFunction and GLFeatures.EXT_fog_coord then
    begin
      { This is not needed for shader pipeline. When we know that rendering
        is done through shaders, then Shader.ModifyFog does everything necessary. }

      { When Renderer.FogVolumetric, GL_FOG_COORDINATE_SOURCE_EXT
        is already set correctly by RenderBegin. }
      if not Renderer.FogVolumetric then
        glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FOG_COORDINATE_EXT);

      glPushAttrib(GL_FOG_BIT);
      glFogi(GL_FOG_MODE, GL_LINEAR); // saved by GL_FOG_BIT
      glFogf(GL_FOG_START, 0); // saved by GL_FOG_BIT
      glFogf(GL_FOG_END, 1); // saved by GL_FOG_BIT
    end;
    {$endif}

    { We potentially override the fog already set
      by TGLRenderer.RenderShapeFog here. }
    Shader.ModifyFog(ftLinear, fcPassedCoordinate, 1, { doesn't matter, it's for ftExp only } 1 );
  end;
end;

procedure TFogCoordinateRenderer.RenderCoordinateEnd;
begin
  {$ifndef OpenGLES}
  { Restore defaults }
  if Arrays.HasFogCoord and
     Arrays.FogDirectValues and
     GLFeatures.EnableFixedFunction and
     GLFeatures.EXT_fog_coord then
  begin
    glPopAttrib;

    { Restore GL_FOG_COORDINATE_SOURCE_EXT. Actually, it seems glPopAttrib
      also would restore it, but I didn't find it in the docs, so don't depend
      on it. }
    if not Renderer.FogVolumetric then
      glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FRAGMENT_DEPTH_EXT);
  end;
  {$endif}

  inherited;
end;

{ TColorPerVertexCoordinateRenderer --------------------------------------- }

procedure TColorPerVertexCoordinateRenderer.RenderCoordinateBegin;
begin
  inherited;

  { The shader implementation of color-per-vertex (for OpenGLES,
    or OpenGL shader pipeline) must be enabled regardless
    if we have lighting or not, as in fact this activates COLOR_PER_VERTEX
    feature, which works regardless if we're lit or unlit.

    However, we can use it only for mcColorPerVertex case (the case
    of mcDefaultColor is implemented in OpenGLES differently,
    without default color "pretending" to be a color per vertex,
    as in fixed-function pipeline case). }
  if MeshColor = mcColorPerVertex then
    Shader.EnableColorPerVertex;

  {$ifndef OpenGLES}
  { for fixed-function pipeline, GL_COLOR_MATERIAL is similar to
    the Shader.EnableColorPerVertex feature, but not exactly the same.
    It can only be used when Lighting, and it suitable for mcDefaultColor too. }
  MaterialFromColorEnabled :=
    GLFeatures.EnableFixedFunction and (MeshColor <> mcNone) and Lighting;
  if MaterialFromColorEnabled then
    { Note: glColorMaterial is already set by TGLRenderer.RenderBegin }
    glEnable(GL_COLOR_MATERIAL);
  {$endif}
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
var
  UseShaderRendering: boolean;
  CurrentX3DProgram: TX3DShaderProgram;
begin
  inherited;

  if Attributes.CustomShader <> nil then
  begin
    if (Attributes.CustomShaderAlphaTest <> nil) and
       Renderer.FixedFunctionAlphaTest then
      CurrentMeshShader := Attributes.CustomShaderAlphaTest
    else
      CurrentMeshShader := Attributes.CustomShader;
  end else
  begin
    if (Shape.Node <> nil) and
       (Shape.Node.Appearance <> nil) then
      Shader.EnableAppearanceEffects(Shape.Node.Appearance.FdEffects);
    if Shape.State.Effects <> nil then
      Shader.EnableGroupEffects(Shape.State.Effects);

    UseShaderRendering := {$ifdef OpenGLES} true {$else}
      (not GLFeatures.EnableFixedFunction) or
      ((Attributes.Mode = rmFull) and Shader.ShapeRequiresShaders)
      {$endif};

    if UseShaderRendering then
    begin
      { calculate and use Shape.ProgramCache[Renderer.Pass].ShaderProgram }
      if (Shape.ProgramCache[Renderer.Pass] = nil) or
         (Shape.ProgramCache[Renderer.Pass].Hash <> Shader.CodeHash) then
      begin
        if Log and LogRenderer then
          WritelnLog('GLSL', 'Creating shaders for shape "%s" (new: %s, lights outside VRML/X3D: %d, pass: %d)',
            [Shape.NiceName,
             BoolToStr((Shape.ProgramCache[Renderer.Pass] = nil) or
                       (Shape.ProgramCache[Renderer.Pass].ShaderProgram = nil), true),
             Renderer.BaseLights.Count,
             Renderer.Pass]);

        if Shape.ProgramCache[Renderer.Pass] <> nil then
          Renderer.Cache.Program_DecReference(Shape.ProgramCache[Renderer.Pass]);

        Shape.ProgramCache[Renderer.Pass] := Renderer.Cache.Program_IncReference(
          Renderer, Shader, Shape.NiceName);
      end;

      Assert(Shape.ProgramCache[Renderer.Pass] <> nil);
      CurrentX3DProgram := Shape.ProgramCache[Renderer.Pass].ShaderProgram;
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

  TGLSLProgram.Current := CurrentMeshShader; // enable the CurrentMeshShader for OpenGL
end;
