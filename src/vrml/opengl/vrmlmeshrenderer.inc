{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ VRML / X3D rendering.
  This is internal for VRMLGLRenderer unit.

  Some ideas for implementation:

  - VRMLGLRenderer unit, and TVRMLGLRenderer class are already
    large and complicated. The idea of this file is to take some tasks
    from their hands. TVRMLMeshRenderer and descendants are created
    only for the time of one RenderShape call, and are used
    to render specific non-trivial VRML mesh nodes.

  - Since all classes within this file and VRMLGLRenderer live
    in one Pascal unit, we could break it and access each other's
    private fields etc. We try to not do it. We try to have a clean
    documented interface. Experience with TGeneralIndexedRenderer
    taught me that this has to be really flexible and designed for
    long-term work, to encompass many nodes and features. So, while
    this is internal for VRMLGLRenderer, it still must be implemented
    and documented cleanly.

  - In the future, this may be abstracted from OpenGL and used as a basis
    also for TVRMLGeometryNode.Triangulate methods implementation.
}

type
  TVRMLMeshRenderer = class
  private
    FRenderer: TVRMLGLRenderer;
    FShape: TVRMLRendererShape;
    FGeometry: TVRMLGeometryNode;
    FState: TVRMLGraphTraverseState;
    FAttributes: TVRMLRenderingAttributes;
  protected
    { Current rendering properties, constant for the whole
      lifetime of the renderer, set in constructor. }
    property Renderer: TVRMLGLRenderer read FRenderer;
    property Shape: TVRMLRendererShape read FShape;
    property Geometry: TVRMLGeometryNode read FGeometry;
    property State: TVRMLGraphTraverseState read FState;
    property Attributes: TVRMLRenderingAttributes read FAttributes;
  public
    { If > 0, RenderShape will not actually render, only prepare
      per-shape resources for fast rendering (arrays and vbos). }
    PrepareRenderShape: Cardinal;

    constructor Create(ARenderer: TVRMLGLRenderer; AShape: TVRMLRendererShape);

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
    (Both IndexedXxx and not indexed). Uses TVRMLArraysGenerator
    instance to generate TGeometryArrays contents, then renders them. }
  TBaseCoordinateRenderer = class(TVRMLMeshRenderer)
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
    Shader: TVRMLShader;

    { Used (and updated) to the number of OpenGL texture units reserved. }
    BoundTextureUnits: Cardinal;

    Lighting: boolean;

    { Don't override this, it is already overridden here
      to do everything necessary. }
    procedure Render; override;
  end;

  TTextureCoordinateRenderer = class(TBaseCoordinateRenderer)
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

{ TVRMLMeshRenderer ---------------------------------------------------------- }

constructor TVRMLMeshRenderer.Create(ARenderer: TVRMLGLRenderer;
  AShape: TVRMLRendererShape);
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
  Locations: TDynLongIntArray;

  procedure EnableVertexAttrib(GLSLProgram: TGLSLProgram);
  var
    I: Integer;
  begin
    Locations := TDynLongIntArray.Create;
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
          (for the same reason as uniform values, see VRMLShader). }
        if not Arrays.Attribs[I].Internal then
          VRMLWarning(vwSerious, E.Message);
    end;
  end;

  procedure DisableArrays;
  var
    I: Integer;
  begin
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_NORMAL_ARRAY);

    if not Attributes.PureGeometry then
      glDisableClientState(GL_COLOR_ARRAY);

    if Arrays.HasFogCoord and GL_EXT_fog_coord then
      glDisableClientState(GL_FOG_COORDINATE_ARRAY_EXT);

    if GLUseMultiTexturing then
    begin
      for I := 0 to Arrays.TexCoords.Count - 1 do
        if (Arrays.TexCoords[I] <> nil) and
           (Arrays.TexCoords[I].Generation = tgExplicit) then
        begin
          glClientActiveTextureARB(GL_TEXTURE0_ARB + I);
          glDisableClientState(GL_TEXTURE_COORD_ARRAY);
        end;
    end else
    if (Arrays.TexCoords.Count <> 0) and
       (Arrays.TexCoords[0] <> nil) and
       (Arrays.TexCoords[0].Generation = tgExplicit) then
    begin
      glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    end;

    if Locations <> nil then
    begin
      for I := 0 to Locations.Count - 1 do
        TGLSLProgram.DisableVertexAttribArray(Locations.Items[I]);
      FreeAndNil(Locations);
    end;
  end;

const
  ToMode: array [TGeometryPrimitive] of TGLenum =
  ( GL_TRIANGLES, GL_QUADS, GL_TRIANGLE_FAN, GL_TRIANGLE_STRIP,
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
        glBindBufferARB(GL_ARRAY_BUFFER_ARB, Vbo[vtCoordinate]);

      glEnableClientState(GL_VERTEX_ARRAY);
      glVertexPointer(3, GL_FLOAT, Arrays.CoordinateSize, Arrays.Position);

      glEnableClientState(GL_NORMAL_ARRAY);
      glNormalPointer(GL_FLOAT, Arrays.CoordinateSize, Arrays.Normal);

      if Vbo[vtAttribute] <> 0 then
        glBindBufferARB(GL_ARRAY_BUFFER_ARB, Vbo[vtAttribute]);

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

      if Arrays.HasFogCoord and GL_EXT_fog_coord then
      begin
        glEnableClientState(GL_FOG_COORDINATE_ARRAY_EXT);
        glFogCoordPointerEXT(GL_FLOAT, Arrays.AttributeSize, Arrays.FogCoord);
      end;

      if GLUseMultiTexturing then
      begin
        for I := 0 to Arrays.TexCoords.Count - 1 do
          if (Arrays.TexCoords[I] <> nil) and
             (Arrays.TexCoords[I].Generation = tgExplicit) then
          begin
            glClientActiveTextureARB(GL_TEXTURE0_ARB + I);
            glEnableClientState(GL_TEXTURE_COORD_ARRAY);
            glTexCoordPointer(Arrays.TexCoords[I].Dimensions, GL_FLOAT,
              Arrays.AttributeSize,
              Arrays.TexCoord(Arrays.TexCoords[I].Dimensions, I, 0));
          end;
      end else
      if (Arrays.TexCoords.Count <> 0) and
         (Arrays.TexCoords[0] <> nil) and
         (Arrays.TexCoords[0].Generation = tgExplicit) then
      begin
        glEnableClientState(GL_TEXTURE_COORD_ARRAY);
        glTexCoordPointer(Arrays.TexCoords[0].Dimensions, GL_FLOAT,
          Arrays.AttributeSize,
          Arrays.TexCoord(Arrays.TexCoords[0].Dimensions, 0, 0));
      end;

      { We know now that CurrentProgram is the program set by
        TShaderCoordinateRenderer.RenderCoordinateBegin }
      if (Arrays.Attribs.Count <> 0) and (CurrentProgram <> nil) then
        EnableVertexAttrib(CurrentProgram);

      Mode := ToMode[Arrays.Primitive];
      First := 0;
      if Arrays.HasIndexes then
      begin
        if Vbo[vtIndex] <> 0 then
          glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, Vbo[vtIndex]);

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
        2. I don't know if it really works for GLSL vertex attribs arrays. }
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

  Renderer.SmoothShading := not Arrays.ForceFlatShading;
end;

procedure TBaseCoordinateRenderer.RenderCoordinateEnd;
begin
end;

function TBaseCoordinateRenderer.HasColor: boolean;
begin
  Result := (not Attributes.PureGeometry) and
    (Arrays.HasColor or Arrays.HasDefaultColor);
end;

{ TTextureCoordinateRenderer ----------------------------------------- }

procedure TTextureCoordinateRenderer.RenderCoordinateBegin;

  { If ARB_multitexturing available, sets texture coordinate for texture
    unit TextureUnit (by appropriate glMultiTexCoord).
    Otherwise (when no multitexturing), sets texture coordinate for
    the only texture unit (glTexCoord).

    The only thing that you have to care about is to specify TextureUnit <
    FreeGLTexturesCount. Everything else (whether ARB_multitexturing
    exists, and shifting TextureUnit by GL_TEXTURE0_ARB) is taken care of inside here. }
  procedure MultiTexCoord(const TextureUnit: Cardinal; const TexCoord: TVector4f);
  begin
    if GL_ARB_multitexture then
      glMultiTexCoordv(GL_TEXTURE0_ARB + TextureUnit, TexCoord) else
      glTexCoordv(TexCoord);
  end;

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

      glMatrixMode(GL_TEXTURE);
      Renderer.PushTextureUnit(TexUnit);
      { CameraMatrix transforms from world space to camera space,
        we want to transform directions from camera space to world
        space below. So inverted CameraMatrix is exactly what we need. }
      RenderingCamera.InverseMatrixNeeded;
      glMultMatrix(RenderingCamera.InverseMatrix);
      glMatrixMode(GL_MODELVIEW);

      if GLVersion.IsMesa and Log then
        { Mesa 7.0.4 makes textures strange (like each cube face was
          1-pixel wide?, although I checked --- they are loaded with
          correct sizes). Output this on log, since it's done by renderer,
          potentially many times for a frame.
          Nothing more intelligent now than warning about this is done. }
        WritelnLog('TexCoord', Format('Warning: Transforming tex coords to WORLDSPACE* under Mesa3D may mess up static cube maps (ImageCubeMapTexture and ComposedCubeMapTexture, but not GeneratedCubeMapTexure)',
          [SuccessName, FailName]));
    end;

  const
    ProjectorScalingMatrix: TMatrix4Single =
    ( (0.5,   0,   0, 0),
      (  0, 0.5,   0, 0),
      (  0,   0, 0.5, 0),
      (0.5, 0.5, 0.5, 1) );
  var
    ProjectorMatrix: TMatrix4Single;
  begin
    Renderer.ActiveTexture(TexUnit);
    case TexCoord.Generation of
      tgSphereMap:
        begin
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
          Shader.EnableTexGen(TexUnit, tgSphere);
        end;
      tgCoord:
        begin
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_S, GL_OBJECT_PLANE, Vector4Single(1, 0, 0, 0));
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_T, GL_OBJECT_PLANE, Vector4Single(0, 1, 0, 0));
          glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_R, GL_OBJECT_PLANE, Vector4Single(0, 0, 1, 0));
          Shader.EnableTexGen(TexUnit, tgObject, 0);
          Shader.EnableTexGen(TexUnit, tgObject, 1);
          Shader.EnableTexGen(TexUnit, tgObject, 2);
        end;
      tgCoordEye:
        begin
          { Vectors given to EYE_PLANE are transformed by inverted
            modelview matrix (at the moment of glTexGen call).
            We don't want to transform them, so just temporarily setting
            modelview matrix to identity makes things work Ok. }
          glPushMatrix;
            glLoadIdentity;
            glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
            glTexGenv(GL_S, GL_EYE_PLANE, Vector4Single(1, 0, 0, 0));
            glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
            glTexGenv(GL_T, GL_EYE_PLANE, Vector4Single(0, 1, 0, 0));
            glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
            glTexGenv(GL_R, GL_EYE_PLANE, Vector4Single(0, 0, 1, 0));
          glPopMatrix;
          Shader.EnableTexGen(TexUnit, tgEye, 0);
          Shader.EnableTexGen(TexUnit, tgEye, 1);
          Shader.EnableTexGen(TexUnit, tgEye, 2);
        end;
      tgCameraSpaceNormal,
      tgWorldSpaceNormal:
        begin
          if not GL_ARB_texture_cube_map then
          begin
            VRMLWarning(vwSerious, 'ARB_texture_cube_map not supported by your OpenGL implementation, cannot generate texture coordinates with mode = "CAMERASPACENORMAL", will do "COORD" instead');
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
          begin
            glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);
            glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);
            glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);

            if TexCoord.Generation = tgWorldSpaceNormal then
              TextureTransformToWorldSpace('WORLDSPACENORMAL', 'CAMERASPACENORMAL');

            Shader.EnableTexGen(TexUnit, tgNormal);
          end;
        end;
      tgCameraSpaceReflectionVector,
      tgWorldSpaceReflectionVector:
        begin
          if not GL_ARB_texture_cube_map then
          begin
            VRMLWarning(vwSerious, 'ARB_texture_cube_map not supported by your OpenGL implementation, cannot generate texture coordinates with mode = "CAMERASPACEREFLECTIONVECTOR", will do "COORD" instead');
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
          begin
            glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);
            glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);
            glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);

            if TexCoord.Generation = tgWorldSpaceReflectionVector then
              TextureTransformToWorldSpace('WORLDSPACEREFLECTIONVECTOR', 'CAMERASPACEREFLECTIONVECTOR');

            Shader.EnableTexGen(TexUnit, tgReflection);
          end;
        end;
      tgProjection:
        begin
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
        end;
      tgBounds2d:
        begin
          { Setup OpenGL to generate tex coords automatically }
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_S, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[0]);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_T, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[1]);
          Shader.EnableTexGen(TexUnit, tgObject, 0);
          Shader.EnableTexGen(TexUnit, tgObject, 1);
        end;
      tgBounds3d:
        begin
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_S, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[0]);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_T, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[1]);
          glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_R, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[2]);
          Shader.EnableTexGen(TexUnit, tgObject, 0);
          Shader.EnableTexGen(TexUnit, tgObject, 1);
          Shader.EnableTexGen(TexUnit, tgObject, 2);
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
var
  I: Integer;
begin
  { Disable OpenGL GL_TEXTURE_GEN_x, if it was (possibly)
    enabled by RenderCoordinateBegin. }
  for I := 0 to Arrays.TexCoords.Count - 1 do
    if Arrays.TexCoords[I].Generation <> tgExplicit then
      Shader.DisableTexGen(I);

  inherited;
end;

{ TFogCoordinateRenderer ------------------------------------------------------- }

procedure TFogCoordinateRenderer.RenderCoordinateBegin;
begin
  inherited;

  if Arrays.HasFogCoord and
     Arrays.FogDirectValues and
     GL_EXT_fog_coord then
  begin
    { When Renderer.FogVolumetric, GL_FOG_COORDINATE_SOURCE_EXT
      is already set correctly by RenderBegin. }
    if not Renderer.FogVolumetric then
      glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FOG_COORDINATE_EXT);

    glPushAttrib(GL_FOG_BIT);
    glFogi(GL_FOG_MODE, GL_LINEAR); // saved by GL_FOG_BIT
    glFogf(GL_FOG_START, 0); // saved by GL_FOG_BIT
    glFogf(GL_FOG_END, 1); // saved by GL_FOG_BIT

    { We potentially override the fog already set
      by TVRMLGLRenderer.RenderShapeFog here. Not a problem
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
     GL_EXT_fog_coord then
  begin
    glPopAttrib;

    { Restore GL_FOG_COORDINATE_SOURCE_EXT. Actually, it seems glPopAttrib
      also would restore it, but I didn't find it in the docs, so don't depend
      on it. }
    if not Renderer.FogVolumetric then
      glFogi(GL_FOG_COORDINATE_SOURCE_EXT, GL_FRAGMENT_DEPTH_EXT);
  end;
  inherited;
end;

{ TMaterialFromColorCoordinateRenderer --------------------------------------- }

procedure TMaterialFromColorCoordinateRenderer.RenderCoordinateBegin;
begin
  inherited;

  MaterialFromColorEnabled := HasColor and Lighting;
  if MaterialFromColorEnabled then
    Shader.EnableMaterialFromColor;
end;

procedure TMaterialFromColorCoordinateRenderer.RenderCoordinateEnd;
begin
  if MaterialFromColorEnabled then
    glDisable(GL_COLOR_MATERIAL);
  inherited;
end;

{ TShaderCoordinateRenderer -------------------------------------------------- }

procedure TShaderCoordinateRenderer.RenderCoordinateBegin;
var
  UseShaderRendering: boolean;
begin
  inherited;

  if (Shape.Node <> nil) and
     (Shape.Node.Appearance <> nil) then
    Shader.EnableAppearanceEffects(Shape.Node.Appearance.FdEffects);
  if Shape.State.Effects <> nil then
    Shader.EnableGroupEffects(Shape.State.Effects);

  UseShaderRendering := (not Attributes.PureGeometry) and
    ( ((Attributes.Shaders = srWhenRequired) and Shader.ShapeRequiresShaders) or
       (Attributes.Shaders = srAlways) );

  if UseShaderRendering then
  begin
    { calculate and use Shape.ProgramCache.ShaderProgram }
    if (Shape.ProgramCache = nil) or
       (Shape.ProgramCache.Hash <> Shader.CodeHash) then
    begin
      if Log then
        WritelnLog('GLSL', 'Creating shaders for shape "%s" (new: %s, lights outside VRML/X3D: %d)',
          [Shape.NiceName,
           BoolToStr[(Shape.ProgramCache = nil) or
                     (Shape.ProgramCache.ShaderProgram = nil)],
           Renderer.BaseLights.Count]);

      if Shape.ProgramCache <> nil then
        Renderer.Cache.Program_DecReference(Shape.ProgramCache);

      Shape.ProgramCache := Renderer.Cache.Program_IncReference(
        Renderer, Shader, Shape.NiceName);
    end;

    Assert(Shape.ProgramCache <> nil);
    CurrentProgram := Shape.ProgramCache.ShaderProgram;
  end else
    CurrentProgram := nil;

  if CurrentProgram <> nil then
    CurrentProgram.SetupUniforms(BoundTextureUnits);
end;
