{
  Copyright 2002-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ X3D rendering.
  This is internal for GLRenderer unit.

  Some ideas for implementation:

  - GLRenderer unit, and TGLRenderer class are already
    large and complicated. The idea of this file is to take some tasks
    from their hands. TMeshRenderer and descendants are created
    only for the time of one RenderShape call, and are used
    to render specific non-trivial X3D mesh nodes.

  - Since all classes within this file and GLRenderer live
    in one Pascal unit, we could break it and access each other's
    private fields etc. We try to not do it. We try to have a clean
    documented interface. Experience with TGeneralIndexedRenderer
    taught me that this has to be really flexible and designed for
    long-term work, to encompass many nodes and features. So, while
    this is internal for GLRenderer, it still must be implemented
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

  { Base abstract class for rendering nodes with explicit "coord" field.
    (Both IndexedXxx and not indexed). Uses TArraysGenerator
    instance to generate TGeometryArrays contents, then renders them. }
  TBaseCoordinateRenderer = class(TMeshRenderer)
  private
    { Available inside RenderCoordinate* }
    Arrays: TGeometryArrays;

    function HasColor: boolean;
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
  private
    RestoreDefaultTexCoord: boolean;
  protected
    procedure RenderCoordinateBegin; override;
    procedure RenderCoordinateEnd; override;
  end;

  TFogCoordinateRenderer = class(TTextureCoordinateRenderer)
  protected
    procedure RenderCoordinateBegin; override;
    procedure RenderCoordinateEnd; override;
  end;

  TMaterialFromColorCoordinateRenderer = class(TFogCoordinateRenderer)
  private
    MaterialFromColorEnabled: boolean;
  protected
    procedure RenderCoordinateBegin; override;
    procedure RenderCoordinateEnd; override;
  end;

  TShaderCoordinateRenderer = class(TMaterialFromColorCoordinateRenderer)
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
  Locations: TLongIntList;

  procedure EnableVertexAttrib(GLSLProgram: TGLSLProgram);
  var
    I: Integer;
  begin
    if Locations = nil then
      Locations := TLongIntList.Create;
    for I := 0 to Arrays.Attribs.Count - 1 do
    try
      case Arrays.Attribs[I].AType of
        atFloat  : Locations.Add(GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 0, 1, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I])));
        atVector2: Locations.Add(GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 0, 2, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I])));
        atVector3: Locations.Add(GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 0, 3, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I])));
        atVector4: Locations.Add(GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 0, 4, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I])));
        atMatrix3:
          begin
            Locations.Add(GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 0, 3, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I])));
            Locations.Add(GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 1, 3, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I], SizeOf(TVector3Single))));
            Locations.Add(GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 2, 3, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I], SizeOf(TVector3Single) * 2)));
          end;
        atMatrix4:
          begin
            Locations.Add(GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 0, 4, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I])));
            Locations.Add(GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 1, 4, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I], SizeOf(TVector4Single))));
            Locations.Add(GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 2, 4, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I], SizeOf(TVector4Single) * 2)));
            Locations.Add(GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 3, 4, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I], SizeOf(TVector4Single) * 3)));
          end;
        else raise EInternalError.Create('Arrays.Attribs[I].AType?');
      end;
    except
      on E: EGLSLAttributeNotFound do
        { Do not make warnings about missing internal attributes
          (for the same reason as uniform values, see GLRendererShader). }
        if not Arrays.Attribs[I].Internal then
          OnWarning(wtMajor, 'VRML/X3D', E.Message);
    end;
  end;

  { Add vertex attribute to shader, ignoring if the attribute
    does not exist (which may happen in case shader doesn't use it,
    e.g. castle_Normal is unused if not lit). }
  procedure OptionalVertexAttrib(const Name: string; LocationOffset: TGLint;
    Size: TGLint; AType: TGLenum; Normalized: TGLboolean; Stride: TGLsizei;
    Ptr: Pointer);
  begin
    try
      Locations.Add(CurrentProgram.VertexAttribPointer(
        Name, LocationOffset, Size, AType, Normalized, Stride, Ptr));
    except
      on E: EGLSLAttributeNotFound do
        { WritelnLog('OpenGLES', E.Message) };
    end;
  end;

  procedure DisableArrays;
  var
    I: Integer;
  begin
    {$ifndef OpenGLES} // TODO-es
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

    if Locations <> nil then
    begin
      for I := 0 to Locations.Count - 1 do
        TGLSLProgram.DisableVertexAttribArray(Locations[I]);
      FreeAndNil(Locations);
    end;
  end;

const
  ToMode: array [TGeometryPrimitive] of TGLenum =
  ( GL_TRIANGLES, {$ifdef OpenGLES} GL_TRIANGLE_FAN { TODO-es } {$else} GL_QUADS {$endif},
    GL_TRIANGLE_FAN, GL_TRIANGLE_STRIP,
    GL_LINE_STRIP, GL_POINTS );
var
  I: Integer;
  First: Cardinal;
  Mode: TGLenum;
begin
  Locations := nil;

  RenderCoordinateBegin;
  try
    if PrepareRenderShape = 0 then
    begin
      { Initialize vertex arrays that we we will use with indexed nodes. }
      if Vbo[vtCoordinate] <> 0 then
        glBindBuffer(GL_ARRAY_BUFFER, Vbo[vtCoordinate]);

      {$ifdef OpenGLES}
      Assert(CurrentProgram <> nil, 'OpenGL ES 2.0 rendering must go through shaders');

      CurrentProgram.SetUniform('castle_ModelViewMatrix', Shape.ModelView);
      CurrentProgram.SetUniform('castle_ProjectionMatrix', ProjectionMatrix);
      CurrentProgram.SetUniform('castle_NormalMatrix', ModelViewToNormalMatrix(Shape.ModelView));
      CurrentProgram.SetUniform('castle_MaterialDiffuseAlpha', Shader.MaterialDiffuse[3]);
      CurrentProgram.SetUniform('castle_MaterialShininess', Shader.MaterialShininessExp);
      CurrentProgram.SetUniform('castle_SceneColor',
        Vector3SingleCut(Shader.MaterialEmission) +
        GlobalAmbient * Vector3SingleCut(Shader.MaterialAmbient));
      CurrentProgram.SetUniform('castle_UnlitColor', Shader.MaterialUnlit);

      Locations := TLongIntList.Create;
      Locations.Add(CurrentProgram.VertexAttribPointer(
        'castle_Vertex', 0, 3, GL_FLOAT, GL_FALSE, Arrays.CoordinateSize, Arrays.Position));
      OptionalVertexAttrib(
        'castle_Normal', 0, 3, GL_FLOAT, GL_FALSE, Arrays.CoordinateSize, Arrays.Normal);
      {$else}
      glEnableClientState(GL_VERTEX_ARRAY);
      glVertexPointer(3, GL_FLOAT, Arrays.CoordinateSize, Arrays.Position);

      glEnableClientState(GL_NORMAL_ARRAY);
      glNormalPointer(GL_FLOAT, Arrays.CoordinateSize, Arrays.Normal);
      {$endif}

      if Vbo[vtAttribute] <> 0 then
        glBindBuffer(GL_ARRAY_BUFFER, Vbo[vtAttribute]);

      {$ifdef OpenGLES}
      if HasColor then
      begin
        if Arrays.HasColor then
        begin
          OptionalVertexAttrib('castle_ColorPerVertex', 0,
            4, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.Color);
        end { TODO-es: else
        if Arrays.HasDefaultColor then
          glColorv(Arrays.DefaultColor); }
      end;

      for I := 0 to Arrays.TexCoords.Count - 1 do
        if (Arrays.TexCoords[I] <> nil) and
           (Arrays.TexCoords[I].Generation = tgExplicit) then
        begin
          OptionalVertexAttrib(
            Format('castle_MultiTexCoord%d', [I]),
            0, Arrays.TexCoords[I].Dimensions, GL_FLOAT, GL_FALSE,
            Arrays.AttributeSize, Arrays.TexCoord(I, 0));
        end;
      {$else}

      if HasColor then
      begin
        if Arrays.HasColor then
        begin
          glEnableClientState(GL_COLOR_ARRAY);
          glColorPointer(4, GL_FLOAT, Arrays.AttributeSize, Arrays.Color);
        end else
        if Arrays.HasDefaultColor then
          glColorv(Arrays.DefaultColor);
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
      {$endif}

      { We know now that CurrentProgram is the program set by
        TShaderCoordinateRenderer.RenderCoordinateBegin }
      if (Arrays.Attribs.Count <> 0) and (CurrentProgram <> nil) then
        EnableVertexAttrib(CurrentProgram);

      Mode := ToMode[Arrays.Primitive];
      First := 0;
      if Arrays.HasIndexes then
      begin
        if Vbo[vtIndex] <> 0 then
          glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, Vbo[vtIndex]);

        if Arrays.Counts = nil then
          glDrawElements(Mode, Arrays.IndexesCount, GL_UNSIGNED_INT, Arrays.IndexesPtr(First)) else
          for I := 0 to Arrays.Counts.Count - 1 do
          begin
            glDrawElements(Mode, Arrays.Counts[I], GL_UNSIGNED_INT, Arrays.IndexesPtr(First));
            First += Arrays.Counts[I];
          end;
      end else
      begin
        if Arrays.Counts = nil then
          glDrawArrays(Mode, First, Arrays.Count) else
          for I := 0 to Arrays.Counts.Count - 1 do
          begin
            glDrawArrays(Mode, First, Arrays.Counts[I]);
            First += Arrays.Counts[I];
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
  { Always make sure Renderer.CullFace is suitable }
  if Arrays.CullBackFaces then
  begin
    { If vertex ordering is consistent and object is SOLID than we use OpenGL's
      backface culling. If FrontFaceCcw then we have to cull CW faces. }
    if Arrays.FrontFaceCcw then
      Renderer.CullFace := cfCW else
      Renderer.CullFace := cfCCW;
  end else
    Renderer.CullFace := cfNone;

  if Renderer.Attributes.Mode = rmFull then
    Renderer.SmoothShading := not Arrays.ForceFlatShading;
end;

procedure TBaseCoordinateRenderer.RenderCoordinateEnd;
begin
end;

function TBaseCoordinateRenderer.HasColor: boolean;
begin
  Result := (Attributes.Mode = rmFull) and
    (Arrays.HasColor or Arrays.HasDefaultColor);
end;

{ TTextureCoordinateRenderer ----------------------------------------- }

{ If multitexturing available, sets texture coordinate for texture
  unit TextureUnit (by appropriate glMultiTexCoord).
  Otherwise (when no multitexturing), sets texture coordinate for
  the only texture unit (glTexCoord).

  The only thing that you have to care about is to specify TextureUnit <
  GLMaxTextureUnits. Everything else (whether multitexturing
  available, and shifting TextureUnit by GL_TEXTURE0) is taken care of inside here. }
procedure MultiTexCoord(const TextureUnit: Cardinal; const TexCoord: TVector4f);
begin
{$ifndef OpenGLES} // TODO-es
  if GLFeatures.UseMultiTexturing then
    glMultiTexCoord4fv(GL_TEXTURE0 + TextureUnit, @TexCoord) else
    glTexCoord4fv(@TexCoord);
{$endif}
end;

procedure TTextureCoordinateRenderer.RenderCoordinateBegin;

  { Enable and set parameters of texture generation,
    for texture units where TexCoords[].Generation[] <> tgExplicit. }
  procedure EnableTextureGen(const TexUnit: Cardinal;
    TexCoord: TGeometryTexCoord);

    { Do the necessary things for WorldSpaceNormal and
      WorldSpaceReflectionVector. }
    procedure TextureTransformToWorldSpace(const SuccessName, FailName: string);
    begin
      { Set the 4th texture coord component (Q) to 0.0.
        Reason: We use matrix 4x4, so it will transform whole texture
        coordinate (4-valued vector, since we're in homogeneous
        coordinates). We want the generated STR to be transformed
        like a direction, so we want to set Q = always 0.0
        (in homogeneous coords, this means that vector represents
        a direction and will be accordingly transformed). }
      MultiTexCoord(TexUnit, ZeroVector4Single);

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

      {$ifndef OpenGLES} // TODO-es
      glMatrixMode(GL_TEXTURE);
      Renderer.PushTextureUnit(TexUnit);
      { CameraMatrix transforms from world space to camera space,
        we want to transform directions from camera space to world
        space below. So inverted CameraMatrix is exactly what we need. }
      RenderingCamera.InverseMatrixNeeded;
      glMultMatrix(RenderingCamera.InverseMatrix);
      glMatrixMode(GL_MODELVIEW);
      {$endif}

      if GLVersion.Mesa and Log then
        { Mesa 7.0.4 makes textures strange (like each cube face was
          1-pixel wide?, although I checked --- they are loaded with
          correct sizes). Output this on log, since it's done by renderer,
          potentially many times for a frame.
          Nothing more intelligent now than warning about this is done. }
        WritelnLog('TexCoord', Format('Warning: Transforming tex coords to WORLDSPACE* under Mesa3D may mess up static cube maps (ImageCubeMapTexture and ComposedCubeMapTexture, but not GeneratedCubeMapTexure)',
          [SuccessName, FailName]));
    end;

  const
    GenerationVectorsIdentity: TTextureGenerationVectors =
    ( (1, 0, 0, 0),
      (0, 1, 0, 0),
      (0, 0, 1, 0) );
  {$ifndef OpenGLES} // TODO-es
  const
    ProjectorScalingMatrix: TMatrix4Single =
    ( (0.5,   0,   0, 0),
      (  0, 0.5,   0, 0),
      (  0,   0, 0.5, 0),
      (0.5, 0.5, 0.5, 1) );
  var
    ProjectorMatrix: TMatrix4Single;
  {$endif}
  begin
    Renderer.ActiveTexture(TexUnit);
    case TexCoord.Generation of
      tgSphereMap:
        Shader.EnableTexGen(TexUnit, tgSphere);
      tgCoord:
        begin
          {$ifndef OpenGLES}
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_S, GL_OBJECT_PLANE, GenerationVectorsIdentity[0]);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_T, GL_OBJECT_PLANE, GenerationVectorsIdentity[1]);
          glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_R, GL_OBJECT_PLANE, GenerationVectorsIdentity[2]);
          {$endif}
          Shader.EnableTexGen(TexUnit, tgObject, 0 {$ifdef OpenGLES}, GenerationVectorsIdentity[0] {$endif});
          Shader.EnableTexGen(TexUnit, tgObject, 1 {$ifdef OpenGLES}, GenerationVectorsIdentity[1] {$endif});
          Shader.EnableTexGen(TexUnit, tgObject, 2 {$ifdef OpenGLES}, GenerationVectorsIdentity[2] {$endif});
        end;
      tgCoordEye:
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
          { TODO: the shader version of this is quite unoptimal,
            we pass 3 uniform vec4 vectors, make 3 dot operations,
            just to simply copy vertex_eye to texture coordinate.
            Same for tgCoord. So far, this is not used often enough
            to bother with optimization. }
          Shader.EnableTexGen(TexUnit, tgEye, 0 {$ifdef OpenGLES}, GenerationVectorsIdentity[0] {$endif});
          Shader.EnableTexGen(TexUnit, tgEye, 1 {$ifdef OpenGLES}, GenerationVectorsIdentity[1] {$endif});
          Shader.EnableTexGen(TexUnit, tgEye, 2 {$ifdef OpenGLES}, GenerationVectorsIdentity[2] {$endif});
        end;
      tgCameraSpaceNormal,
      tgWorldSpaceNormal:
        begin
          {$ifdef OpenGLES}
          Assert(GLFeatures.TextureCubeMap <> gsNone); // on OpenGLES, cubemaps are always supported
          {$else}
          if GLFeatures.TextureCubeMap = gsNone then
          begin
            OnWarning(wtMajor, 'VRML/X3D', 'ARB_texture_cube_map not supported by your OpenGL implementation, cannot generate texture coordinates with mode = "CAMERASPACENORMAL", will do "COORD" instead');
            glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_S, GL_OBJECT_PLANE, Vector4Single(1, 0, 0, 0));
            glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_T, GL_OBJECT_PLANE, Vector4Single(0, 1, 0, 0));
            glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_R, GL_OBJECT_PLANE, Vector4Single(0, 0, 1, 0));

            Shader.EnableTexGen(TexUnit, tgObject, 0);
            Shader.EnableTexGen(TexUnit, tgObject, 1);
            Shader.EnableTexGen(TexUnit, tgObject, 2);
          end else
          {$endif}
          begin
            if TexCoord.Generation = tgWorldSpaceNormal then
              TextureTransformToWorldSpace('WORLDSPACENORMAL', 'CAMERASPACENORMAL');
            Shader.EnableTexGen(TexUnit, tgNormal);
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
            OnWarning(wtMajor, 'VRML/X3D', 'ARB_texture_cube_map not supported by your OpenGL implementation, cannot generate texture coordinates with mode = "CAMERASPACEREFLECTIONVECTOR", will do "COORD" instead');
            glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_S, GL_OBJECT_PLANE, Vector4Single(1, 0, 0, 0));
            glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_T, GL_OBJECT_PLANE, Vector4Single(0, 1, 0, 0));
            glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
            glTexGenv(GL_R, GL_OBJECT_PLANE, Vector4Single(0, 0, 1, 0));

            Shader.EnableTexGen(TexUnit, tgObject, 0);
            Shader.EnableTexGen(TexUnit, tgObject, 1);
            Shader.EnableTexGen(TexUnit, tgObject, 2);
          end else
          {$endif}
          begin
            if TexCoord.Generation = tgWorldSpaceReflectionVector then
              TextureTransformToWorldSpace('WORLDSPACEREFLECTIONVECTOR', 'CAMERASPACEREFLECTIONVECTOR');
            Shader.EnableTexGen(TexUnit, tgReflection);
          end;
        end;
      tgProjection:
        begin
          {$ifndef OpenGLES} // TODO-es
          if Assigned(TexCoord.GenerationProjectorMatrix) then
          begin
            ProjectorMatrix := ProjectorScalingMatrix * TexCoord.GenerationProjectorMatrix();
            glPushMatrix;
              glLoadMatrix(RenderingCamera.Matrix);

              glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
              glTexGenv(GL_S, GL_EYE_PLANE, MatrixRow(ProjectorMatrix, 0));

              glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
              glTexGenv(GL_T, GL_EYE_PLANE, MatrixRow(ProjectorMatrix, 1));

              glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
              glTexGenv(GL_R, GL_EYE_PLANE, MatrixRow(ProjectorMatrix, 2));

              glTexGeni(GL_Q, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
              glTexGenv(GL_Q, GL_EYE_PLANE, MatrixRow(ProjectorMatrix, 3));
            glPopMatrix;
            Shader.EnableTexGen(TexUnit, tgEye, 0);
            Shader.EnableTexGen(TexUnit, tgEye, 1);
            Shader.EnableTexGen(TexUnit, tgEye, 2);
            Shader.EnableTexGen(TexUnit, tgEye, 3);
          end;
          {$endif}
        end;
      tgBounds2d:
        begin
          {$ifndef OpenGLES}
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_S, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[0]);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_T, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[1]);
          {$endif}
          Shader.EnableTexGen(TexUnit, tgObject, 0 {$ifdef OpenGLES}, TexCoord.GenerationBoundsVector[0] {$endif});
          Shader.EnableTexGen(TexUnit, tgObject, 1 {$ifdef OpenGLES}, TexCoord.GenerationBoundsVector[1] {$endif});
        end;
      tgBounds3d:
        begin
          {$ifndef OpenGLES}
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_S, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[0]);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_T, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[1]);
          glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_R, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[2]);
          {$endif}
          Shader.EnableTexGen(TexUnit, tgObject, 0 {$ifdef OpenGLES}, TexCoord.GenerationBoundsVector[0] {$endif});
          Shader.EnableTexGen(TexUnit, tgObject, 1 {$ifdef OpenGLES}, TexCoord.GenerationBoundsVector[1] {$endif});
          Shader.EnableTexGen(TexUnit, tgObject, 2 {$ifdef OpenGLES}, TexCoord.GenerationBoundsVector[2] {$endif});
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
const
  DefaultTexCoord: TVector4Single = (0, 0, 0, 1);
var
  I: Integer;
begin
  for I := 0 to Arrays.TexCoords.Count - 1 do
    if Arrays.TexCoords[I].Generation <> tgExplicit then
    begin
      { Disable OpenGL GL_TEXTURE_GEN_x, if it was (possibly)
        enabled by RenderCoordinateBegin. }
      Shader.DisableTexGen(I);
      { DisableTexGen already set glActiveTexture }
      if RestoreDefaultTexCoord then
        MultiTexCoord(I, DefaultTexCoord);
    end;

  inherited;
end;

{ TFogCoordinateRenderer ------------------------------------------------------- }

procedure TFogCoordinateRenderer.RenderCoordinateBegin;
begin
  inherited;

  if Arrays.HasFogCoord and
     Arrays.FogDirectValues and
     GLFeatures.EXT_fog_coord then
  begin
    {$ifndef OpenGLES} // TODO-es
    { When Renderer.FogVolumetric, GL_FOG_COORDINATE_SOURCE_EXT
      is already set correctly by RenderBegin. }
    if not Renderer.FogVolumetric then
      glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FOG_COORDINATE_EXT);

    glPushAttrib(GL_FOG_BIT);
    glFogi(GL_FOG_MODE, GL_LINEAR); // saved by GL_FOG_BIT
    glFogf(GL_FOG_START, 0); // saved by GL_FOG_BIT
    glFogf(GL_FOG_END, 1); // saved by GL_FOG_BIT
    {$endif}

    { We potentially override the fog already set
      by TGLRenderer.RenderShapeFog here. Not a problem
      (TODO: although previous EnableFog added to hash, which may make
      shader programs cache less shared.) }
    Shader.EnableFog(ftLinear, fcPassedCoordinate);
  end;
end;

procedure TFogCoordinateRenderer.RenderCoordinateEnd;
begin
  { Restore defaults }
  if Arrays.HasFogCoord and
     Arrays.FogDirectValues and
     GLFeatures.EXT_fog_coord then
  begin
    {$ifndef OpenGLES} // TODO-es
    glPopAttrib;

    { Restore GL_FOG_COORDINATE_SOURCE_EXT. Actually, it seems glPopAttrib
      also would restore it, but I didn't find it in the docs, so don't depend
      on it. }
    if not Renderer.FogVolumetric then
      glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FRAGMENT_DEPTH_EXT);
    {$endif}
  end;
  inherited;
end;

{ TMaterialFromColorCoordinateRenderer --------------------------------------- }

procedure TMaterialFromColorCoordinateRenderer.RenderCoordinateBegin;
begin
  inherited;

  MaterialFromColorEnabled := HasColor
    {$ifndef OpenGLES}
    { For OpenGLES, we must call EnableMaterialFromColor regardless
      if we have lighting or not, as in fact this activates COLOR_PER_VERTEX
      feature, which works regardless if we're lit or unlit. }
    and Lighting
    {$endif};
  if MaterialFromColorEnabled then
    Shader.EnableMaterialFromColor;
end;

procedure TMaterialFromColorCoordinateRenderer.RenderCoordinateEnd;
begin
  {$ifndef OpenGLES} // TODO-es
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
      CurrentProgram := Attributes.CustomShaderAlphaTest else
      CurrentProgram := Attributes.CustomShader;
  end else
  begin
    if (Shape.Node <> nil) and
       (Shape.Node.Appearance <> nil) then
      Shader.EnableAppearanceEffects(Shape.Node.Appearance.FdEffects);
    if Shape.State.Effects <> nil then
      Shader.EnableGroupEffects(Shape.State.Effects);

    UseShaderRendering := {$ifdef OpenGLES} true {$else}
      (Attributes.Mode = rmFull) and
      ( ((Attributes.Shaders = srWhenRequired) and Shader.ShapeRequiresShaders) or
         (Attributes.Shaders = srAlways) )
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
             BoolToStr[(Shape.ProgramCache[Renderer.Pass] = nil) or
                       (Shape.ProgramCache[Renderer.Pass].ShaderProgram = nil)],
             Renderer.BaseLights.Count,
             Renderer.Pass]);

        if Shape.ProgramCache[Renderer.Pass] <> nil then
          Renderer.Cache.Program_DecReference(Shape.ProgramCache[Renderer.Pass]);

        Shape.ProgramCache[Renderer.Pass] := Renderer.Cache.Program_IncReference(
          Renderer, Shader, Shape.NiceName);
      end;

      Assert(Shape.ProgramCache[Renderer.Pass] <> nil);
      CurrentX3DProgram := Shape.ProgramCache[Renderer.Pass].ShaderProgram;
      CurrentProgram := CurrentX3DProgram;
      Shader.SetDynamicUniforms(CurrentX3DProgram);
    end else
      CurrentProgram := nil;
  end;

  if CurrentProgram <> nil then
    CurrentProgram.SetupUniforms(BoundTextureUnits);
end;
