{
  Copyright 2002-2010 Michalis Kamburelis.

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
    FState: TVRMLGraphTraverseState;
    FGeometry: TVRMLGeometryNode;
    FAttributes: TVRMLRenderingAttributes;
  protected
    { Current rendering properties, constant for the whole
      lifetime of the renderer, set in constructor.
      Setting Renderer automatically sets Shape, State, Geometry, Attributes. }
    property Renderer: TVRMLGLRenderer read FRenderer;
    property Shape: TVRMLRendererShape read FShape;
    property State: TVRMLGraphTraverseState read FState;
    property Geometry: TVRMLGeometryNode read FGeometry;
    property Attributes: TVRMLRenderingAttributes read FAttributes;
  public
    { Constructor.

      For descendants implementors:
      Don't do any OpenGL state changes here (initialization of OpenGL state
      should go to Render). }
    constructor Create(ARenderer: TVRMLGLRenderer);

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
    { used by RenderCoordinateBegin/End for CullBackFaces and FrontFaceCcw
      implementation. }
    PushedAttribs: TGLbitfield;

    { Available inside RenderCoordinate* }
    Arrays: TGeometryArrays;
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

    { Don't override this, it is already overridden here
      to do everything necessary. }
    procedure Render; override;

    { Generate VBO if needed, and reload VBO contents.
      Assumes GL_ARB_vertex_buffer_object is true.

      Arrays data @italic(must not) be freed (by TGeometryArrays.FreeData)
      before calling this method. Also, this method will always call
      Arrays.FreeData. So do not load the same TGeometryArrays instance
      twice to the Vbo.

      We always keep assertion that Vbo is loaded <=> Arrays data is freed. }
    class procedure LoadArraysToVbo(const AArrays: TGeometryArrays;
      var AVbo: TVboArrays);
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

  TCompleteCoordinateRenderer = TFogCoordinateRenderer;

{ TVRMLMeshRenderer ---------------------------------------------------------- }

constructor TVRMLMeshRenderer.Create(ARenderer: TVRMLGLRenderer);
begin
  inherited Create;

  FRenderer := ARenderer;
  FShape := Renderer.CurrentShape;
  FState := Renderer.CurrentState;
  FGeometry := Renderer.CurrentGeometry;
  FAttributes := Renderer.Attributes;
end;

{ TBaseCoordinateRenderer ------------------------------------------------ }

class procedure TBaseCoordinateRenderer.LoadArraysToVbo(
  const AArrays: TGeometryArrays; var AVbo: TVboArrays);
begin
  Assert(GL_ARB_vertex_buffer_object);
  Assert(not AArrays.DataFreed);

  if AVbo[0] = 0 then
  begin
    glGenBuffersARB(High(AVbo) + 1, @AVbo);
    if Log then
      WritelnLog('Renderer', Format('Creating and loading data to VBOs (%d,%d,%d)',
        [AVbo[0], AVbo[1], AVbo[2]]));
  end else
  begin
    if Log then
      WritelnLog('Renderer', Format('Loading data to existing VBOs (%d,%d,%d)',
        [AVbo[0], AVbo[1], AVbo[2]]));
  end;

  glBindBufferARB(GL_ARRAY_BUFFER_ARB, AVbo[0]);
  glBufferDataARB(GL_ARRAY_BUFFER_ARB, AArrays.Count * AArrays.CoordinateSize,
    AArrays.CoordinateArray, GL_STREAM_DRAW_ARB);

  glBindBufferARB(GL_ARRAY_BUFFER_ARB, AVbo[1]);
  glBufferDataARB(GL_ARRAY_BUFFER_ARB, AArrays.Count * AArrays.AttributeSize,
    AArrays.AttributeArray, GL_STREAM_DRAW_ARB);

  if AArrays.Indexes <> nil then
  begin
    glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, AVbo[2]);
    glBufferDataARB(GL_ELEMENT_ARRAY_BUFFER_ARB, AArrays.Indexes.Count * SizeOf(LongInt),
      AArrays.Indexes, GL_STREAM_DRAW_ARB);
  end;

  AArrays.FreeData;
end;

procedure TBaseCoordinateRenderer.Render;
var
  { This is used only by LockArraysBegin and LockArraysEnd. }
  UseLockArrays: boolean;

  procedure LockArraysBegin;
  begin
    { See
      [http://www.opengl.org/documentation/specs/version1.2/EXTspecs/compiled_vertex_array.txt]
      for description of GL_EXT_compiled_vertex_array. }
    UseLockArrays := (Shape.Vbo[0] = 0) and
      GL_EXT_compiled_vertex_array and
      { Under Radeon X1600 (chantal), fglrx, with roNone optimization,
        rendering some shapes without indexes (like 16000heights from
        http://www.web3d.org/x3d/content/examples/Conformance/Geometry/ElevationGrid/index.html,
        or shadow_fields demo) causes invalid results
        (look like random vertexes). So disable using locking for them. }
      (not (GLVersion.IsFglrx and Arrays.HasIndexes)) and
      { Don't use when Arrays.Count has no items,
        glLockArraysEXT(0, 0) causes OpenGL error "invalid value". }
     (Arrays.Count <> 0);
    if UseLockArrays then
      glLockArraysEXT(0, Arrays.Count);
  end;

  procedure LockArraysEnd;
  begin
    if UseLockArrays then
      glUnlockArraysEXT;
  end;

  procedure EnableVertexAttrib(GLSLProgram: TGLSLProgram);
  var
    I: Integer;
  begin
    for I := 0 to Arrays.Attribs.Count - 1 do
    try
      case Arrays.Attribs[I].AType of
        atFloat  : GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 0, 1, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));
        atVector2: GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 0, 2, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));
        atVector3: GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 0, 3, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));
        atVector4: GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 0, 4, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));
        atMatrix3:
          begin
            GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 0, 3, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));
            GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 1, 3, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I], SizeOf(TVector3Single)));
            GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 2, 3, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I], SizeOf(TVector3Single) * 2));
          end;
        atMatrix4:
          begin
            GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 0, 4, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I]));
            GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 1, 4, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I], SizeOf(TVector4Single)));
            GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 2, 4, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I], SizeOf(TVector4Single) * 2));
            GLSLProgram.VertexAttribPointer(Arrays.Attribs[I].Name, 3, 4, GL_FLOAT, GL_FALSE, Arrays.AttributeSize, Arrays.GLSLAttribute(Arrays.Attribs[I], SizeOf(TVector4Single) * 3));
          end;
        else raise EInternalError.Create('Arrays.Attribs[I].AType?');
      end;
    except
      on E: EGLSLAttributeNotFound do VRMLWarning(vwSerious, E.Message);
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
  MaterialFromColorEnabled: boolean;
begin
  MaterialFromColorEnabled := false;

  RenderCoordinateBegin;
  try

    { Initialize vertex arrays that we we will use with indexed nodes.
      GL_CLIENT_VERTEX_ARRAY_BIT pushes state of every vertex array, including
      normal and texture coord arrays.

      Mesa 7.2 (at least this one) crashes awfully when using
      glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT).
      The crash is a segfault, sometimes from my code, sometimes from
      within the mesa lib, sometimes from GTK... evidently, Mesa's
      glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT) cripples some memory
      leading to crash at some random place later.

      To reproduce: open with view3dscene almost any non-empty model,
      like kambi_vrml_test_suite/x3d/kambi_extensions/teapot_simple.x3dv.
      Sometimes you need to open it a couple of times... and you get this crash.
      Reproducible easily on "chantal" (mac book pro) and "domek"
      (netbook MSI Wind).

      TODO: I don't know is it known bug, in what Mesa version it's produced
      and when (if) it's fixed... for now I'll just avoid doing
      glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT) with Mesa.
      Yes, this makes rendering code buggy (GL_CLIENT_VERTEX_ARRAY_BIT
      "leaks out" from our VRML renderer), but it's not a problem
      in practice for my programs --- I didn't depend anywhere that
      VRML renderer preserves that glVertexPointer state.
    }
    if not GLVersion.IsMesa then
      glPushClientAttrib(GL_CLIENT_VERTEX_ARRAY_BIT);
    try
      if Shape.Vbo[0] <> 0 then
        glBindBufferARB(GL_ARRAY_BUFFER_ARB, Shape.Vbo[0]);

      glEnableClientState(GL_VERTEX_ARRAY);
      glVertexPointer(3, GL_FLOAT, Arrays.CoordinateSize, Arrays.Position);

      glEnableClientState(GL_NORMAL_ARRAY);
      glNormalPointer(GL_FLOAT, Arrays.CoordinateSize, Arrays.Normal);

      if Shape.Vbo[1] <> 0 then
        glBindBufferARB(GL_ARRAY_BUFFER_ARB, Shape.Vbo[1]);

      if Attributes.ControlMaterials and
         (not Attributes.PureGeometry) then
      begin
        MaterialFromColorEnabled := Renderer.MaterialLit;
        if MaterialFromColorEnabled then
        begin
          { We always set diffuse material component from the color.
            This satisfies all cases. }
          glColorMaterial(GL_FRONT_AND_BACK, GL_DIFFUSE);
          glEnable(GL_COLOR_MATERIAL);
        end;

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
            glClientActiveTextureARB(GL_TEXTURE0_ARB +
              Integer(Renderer.Attributes.FirstGLFreeTexture) + I);
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

      if Arrays.Attribs.Count <> 0 then
      begin
        if Renderer.UsedGLSL <> nil then
          EnableVertexAttrib(Renderer.UsedGLSL.GLSLProgram) else
        if Renderer.ShapeBumpMappingUsed <> bmNone then
          EnableVertexAttrib(Renderer.BmGLSLProgram[
            Renderer.ShapeBumpMappingUsed >= bmGLSLParallax]);
      end;

      { Lock arrays after setting up all arrays. }
      LockArraysBegin;
      try
        Mode := ToMode[Arrays.Primitive];
        First := 0;
        if Arrays.HasIndexes then
        begin
          if Shape.Vbo[2] <> 0 then
            glBindBufferARB(GL_ELEMENT_ARRAY_BUFFER_ARB, Shape.Vbo[2]);

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
      finally LockArraysEnd; end;
    finally
      if not GLVersion.IsMesa then
        glPopClientAttrib;
    end;

  finally RenderCoordinateEnd end;

  if MaterialFromColorEnabled then
    glDisable(GL_COLOR_MATERIAL);
end;

procedure TBaseCoordinateRenderer.RenderCoordinateBegin;
begin
  PushedAttribs := 0;

  if Arrays.CullBackFaces then
    PushedAttribs := PushedAttribs or GL_POLYGON_BIT;
  if Arrays.ForceFlatShading then
    PushedAttribs := PushedAttribs or GL_LIGHTING_BIT { for shade model };

  if PushedAttribs <> 0 then
    glPushAttrib(PushedAttribs);

  if Arrays.CullBackFaces then
  begin
    { If vertex ordering is consistent and object is SOLID than we use OpenGL's
      backface culling.

      If FrontFaceCcw then we have to cull CW faces.
      But note that we do not want to call OpenGL glFrontFace
      (see VRMLGLRenderer comments for reasons), so instead of it we switch
      glCullFace. Since we assume that front = always CCW, so we know how to call
      glCullFace.
    }
    glEnable(GL_CULL_FACE);
    if Arrays.FrontFaceCcw then
      glCullFace(GL_BACK) else
      glCullFace(GL_FRONT);
  end;

  if Arrays.ForceFlatShading then
  begin
    glShadeModel(GL_FLAT);
  end;
end;

procedure TBaseCoordinateRenderer.RenderCoordinateEnd;
begin
  if PushedAttribs <> 0 then
    glPopAttrib;
end;

{ TTextureCoordinateRenderer ----------------------------------------- }

{ TODO:
  - maybe move NodeTextured here now? We can query created mesh renderer
    before calling it's render!
}

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
      Renderer.MultiTexCoord(TexUnit, ZeroVector4Single);

      glMatrixMode(GL_TEXTURE);
      Renderer.PushTextureUnit(TexUnit);
      { CameraMatrix transforms from world space to camera space,
        we want to transform directions from camera space to world
        space below. So inverted CameraMatrix is exactly what we need. }
      RenderState.CameraInverseMatrixNeeded;
      glMultMatrix(RenderState.CameraInverseMatrix);
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

  begin
    Renderer.ActiveTexture(TexUnit);
    case TexCoord.Generation of
      tgSphere:
        begin
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_SPHERE_MAP);
          glEnable(GL_TEXTURE_GEN_S);
          glEnable(GL_TEXTURE_GEN_T);
        end;
      tgCoord:
        begin
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_S, GL_OBJECT_PLANE, Vector4Single(1, 0, 0, 0));
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_T, GL_OBJECT_PLANE, Vector4Single(0, 1, 0, 0));
          glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_R, GL_OBJECT_PLANE, Vector4Single(0, 0, 1, 0));
          glEnable(GL_TEXTURE_GEN_S);
          glEnable(GL_TEXTURE_GEN_T);
          glEnable(GL_TEXTURE_GEN_R);
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
          glEnable(GL_TEXTURE_GEN_S);
          glEnable(GL_TEXTURE_GEN_T);
          glEnable(GL_TEXTURE_GEN_R);
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
          end else
          begin
            glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);
            glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);
            glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_NORMAL_MAP_ARB);

            if TexCoord.Generation = tgWorldSpaceNormal then
              TextureTransformToWorldSpace('WORLDSPACENORMAL', 'CAMERASPACENORMAL');
          end;

          glEnable(GL_TEXTURE_GEN_S);
          glEnable(GL_TEXTURE_GEN_T);
          glEnable(GL_TEXTURE_GEN_R);
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
          end else
          begin
            glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);
            glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);
            glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_REFLECTION_MAP_ARB);

            if TexCoord.Generation = tgWorldSpaceReflectionVector then
              TextureTransformToWorldSpace('WORLDSPACEREFLECTIONVECTOR', 'CAMERASPACEREFLECTIONVECTOR');
          end;

          glEnable(GL_TEXTURE_GEN_S);
          glEnable(GL_TEXTURE_GEN_T);
          glEnable(GL_TEXTURE_GEN_R);
        end;
      tgProjection:
        begin
          if TexCoord.HasGenerationProjectorMatrix then
          begin
            glPushMatrix;
              glLoadMatrix(RenderState.CameraMatrix);

              glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
              glTexGenv(GL_S, GL_EYE_PLANE, MatrixRow(TexCoord.GenerationProjectorMatrix, 0));

              glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
              glTexGenv(GL_T, GL_EYE_PLANE, MatrixRow(TexCoord.GenerationProjectorMatrix, 1));

              glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
              glTexGenv(GL_R, GL_EYE_PLANE, MatrixRow(TexCoord.GenerationProjectorMatrix, 2));

              glTexGeni(GL_Q, GL_TEXTURE_GEN_MODE, GL_EYE_LINEAR);
              glTexGenv(GL_Q, GL_EYE_PLANE, MatrixRow(TexCoord.GenerationProjectorMatrix, 3));
            glPopMatrix;
            glEnable(GL_TEXTURE_GEN_S);
            glEnable(GL_TEXTURE_GEN_T);
            glEnable(GL_TEXTURE_GEN_R);
            glEnable(GL_TEXTURE_GEN_Q);
          end;
        end;
      tgBounds2d:
        begin
          { Setup OpenGL to generate tex coords automatically }
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_S, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[0]);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_T, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[1]);
          glEnable(GL_TEXTURE_GEN_S);
          glEnable(GL_TEXTURE_GEN_T);
        end;
      tgBounds3d:
        begin
          glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_S, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[0]);
          glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_T, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[1]);
          glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
          glTexGenv(GL_R, GL_OBJECT_PLANE, TexCoord.GenerationBoundsVector[2]);
          glEnable(GL_TEXTURE_GEN_S);
          glEnable(GL_TEXTURE_GEN_T);
          glEnable(GL_TEXTURE_GEN_R);
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

  procedure DisableTextureGen(const TextureUnit: Cardinal);
  begin
    Renderer.ActiveTexture(TextureUnit);
    glDisable(GL_TEXTURE_GEN_S);
    glDisable(GL_TEXTURE_GEN_T);
    glDisable(GL_TEXTURE_GEN_R);
    glDisable(GL_TEXTURE_GEN_Q);
  end;

var
  I: Integer;
begin
  { Disable OpenGL GL_TEXTURE_GEN_x, if it was (possibly)
    enabled by RenderCoordinateBegin. }
  for I := 0 to Arrays.TexCoords.Count - 1 do
    if Arrays.TexCoords[I].Generation <> tgExplicit then
      DisableTextureGen(I);

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
