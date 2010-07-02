{$ifdef read_interface}

  { Rendering with bump mapping. This requires some special preparations,
    enabling / disabling state and such, so it's natural to have this
    in a nice separated class. It's associated with KambiAppearance node
    defining heightMap, normalMap and other bump mapping properties.

    Previously, some of this state was stored along inside TGLImageTextureNode.
    But this caused many problems:

    @unorderedList(
      @item(
        First of all --- bump mapping doesn't really work with normal
        texturing, as it has to apply multitextuting and shaders by itself.
        Handling it inside vrmltexturerenderer.inc was complicating
        vrmltexturerenderer.inc, adding exceptional cases inside it's code.
        It should be the other way around: bump mapping code should use
        lower-level vrmltexturerenderer.inc to load and manager it's
        normal / height maps. (And it's done this way currently.))

      @item(
        Actual texture was tied to it's normalMap and heightMap,
        e.g. you cauldn't use the same VRML ImageTexture node with different
        normalMap / heightMap in the same VRML file.)
    )

    TODO: for now, bump mapping is used only if the node has normal texture
    too. It should be possible to use bump mapping even if the node is colored
    only by material (in this case we should remember to still generate
    texture coords etc.).
  }
  TBumpMappingRenderer = class(TResourceRenderer)
  private
    class function IsImageTexture(ANode: TVRMLNode): boolean;
    class function IsGLImageTexture(GLNode: TGLTextureNode): boolean;
  public
    { ANode must be TNodeKambiAppearance }
    constructor Create(ARenderer: TVRMLOpenGLRenderer; ANode: TVRMLNode); override;

    function Node: TNodeKambiAppearance;

    { Prepare normalMap, heightMap (if exist) for rendering.
      If some success (there's a chance that bump mapping may be enabled
      for some configuration), prepares other resources for bump mapping too. }
    procedure Prepare(State: TVRMLGraphTraverseState);

    { Release things initialized by Prepare. }
    procedure Unprepare; override;

    { Enable things for rendering with bump mapping.

      As ColorTextureNode pass actual color texture that should be visible.

      Returns success. If @true, normal texture and shaders
      setup should not be done. Bump mapping coords should be generated.
      And you have to call Disable at the end.
      If @false, this did nothing, and you can proceed as usual. }
    function Enable(State: TVRMLGraphTraverseState;
      ColorTextureNode: TGLTextureNode): boolean;

    { Disable things enabled by successfull @link(Enable).
      Note: it assumes MeshRenderer remained the same during this time. }
    procedure Disable;
  end;

  TBumpMappingRenderersList = class(TResourceRenderersList)
  private
    function GetItems(const Index: Integer): TBumpMappingRenderer;
  public
    { Looks for item with given Node.
      Returns -1 if not found. }
    function NodeIndex(ANode: TNodeKambiAppearance): Integer;

    { Looks for item with given Node.
      Returns @nil if not found. }
    function Node(ANode: TNodeKambiAppearance): TBumpMappingRenderer;

    property Items[Index: Integer]: TBumpMappingRenderer read GetItems; default;

    { If this State contains appropriate TNodeKambiAppearance,
      this will create and prepare TBumpMappingRenderer instance for it
      (if one doesn't exist already). }
    procedure Prepare(State: TVRMLGraphTraverseState;
      ARenderer: TVRMLOpenGLRenderer);

    { If this State contains appropriate TNodeKambiAppearance,
      and TBumpMappingRenderer instance is prepared for this Node,
      we will call it's Enable method here.

      We return the TBumpMappingRenderer instance only if it existed
      and it's Enable method returned @true. Otherwise, we return @nil.
      IOW, if we return non-nil, then bump mapping is for sure enabled,
      and you should call Disable on it later. }
    function Enable(State: TVRMLGraphTraverseState;
      ColorTextureNode: TGLTextureNode): TBumpMappingRenderer;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TBumpMappingRenderer.Create(ARenderer: TVRMLOpenGLRenderer; ANode: TVRMLNode);
begin
  Assert(ANode is TNodeKambiAppearance, 'TBumpMappingRenderer.Create acceps as Node only TNodeKambiAppearance');
  inherited;
end;

function TBumpMappingRenderer.Node: TNodeKambiAppearance;
begin
  Result := TNodeKambiAppearance(inherited Node);
end;

class function TBumpMappingRenderer.IsImageTexture(ANode: TVRMLNode): boolean;
begin
  Result :=
    (ANode <> nil) and
    (ANode is TVRMLTextureNode) and
    (not (ANode is TNodeMovieTexture));
end;

class function TBumpMappingRenderer.IsGLImageTexture(GLNode: TGLTextureNode): boolean;
begin
  Result :=
    (GLNode <> nil) and
    (GLNode is TGLImageTextureNode) and
    (TGLImageTextureNode(GLNode).GLName <> 0);
end;

procedure TBumpMappingRenderer.Prepare(State: TVRMLGraphTraverseState);

  { Called when it's detected that bump mapping
    may be actually used. This is supposed to initialize anything related to
    BumpMapping. }
  procedure PrepareBumpMapping(Parallax: boolean);
  var
    ProgramDefines: string;
    BmProgram: TGLSLProgram;
  begin
    case Renderer.BumpMappingMethod of
      bmMultiTexDotNormalized:
        if Renderer.TexNormalizationCube = 0 then
        begin
          Renderer.TexNormalizationCube := MakeNormalizationCubeMap;
        end;

      bmGLSLNormal,
      bmGLSLParallax:
        if Renderer.BmGLSLProgram[Parallax] = nil then
        begin
          BmProgram := TGLSLProgram.Create;

          { If BumpMappingMethod is in bmGLSLAll, then we checked in
            BumpMappingMethod that support is <> gsNone }
          Assert(BmProgram.Support <> gsNone);

          try
            if Parallax then
            begin
              { ATI Mobility Radeon X1600 (on Mac Book Pro, computer "chantal")
                says that
                  #version must occur before any other statement in the program
                At the same time, NVidia requires this #version... To be
                absolutely clean, I just place #version here, at the very
                beginning of shader source. }
              ProgramDefines := '#version 110' + LineEnding;

              if Renderer.BmSteepParallaxMapping then
                ProgramDefines += '#define STEEP' + LineEnding else
                ProgramDefines += '';

              BmProgram.AttachVertexShader(
                ProgramDefines + {$I parallax_bump_mapping.vs.inc});
              BmProgram.AttachFragmentShader(
                ProgramDefines + {$I parallax_bump_mapping.fs.inc});
            end else
            begin
              BmProgram.AttachVertexShader({$I bump_mapping.vs.inc});
              BmProgram.AttachFragmentShader({$I bump_mapping.fs.inc});
            end;

            BmProgram.Link(true);

            if Log then
              WritelnLog('Bump mapping',
                Format('Compiled and linked GLSL program for ' +
                  'bump mapping. Parallax: %s (if true: steep parallax ' +
                  'with self-shadowing: %s).',
                  [BoolToStr[Parallax], BoolToStr[Renderer.BmSteepParallaxMapping]]));
          except
            on E: EGLSLError do
            begin
              if Parallax and Renderer.BmSteepParallaxMapping then
              begin
                { If we failed with compiling/linking steep parallax mapping,
                  retry without BmSteepParallaxMapping.
                  This happens e.g. on NVidia GeForce FX 5200
                  ("kocury home" computer). }
                if Log then
                  WritelnLog('Bump mapping', 'Steep parallax mapping program ' +
                    'not compiled or linked, falling back to classic parallax.');

                FreeAndNil(BmProgram);
                Renderer.BmSteepParallaxMapping := false;
                PrepareBumpMapping(Parallax);
                Exit;
              end else
                raise;
            end;
          end;

          { tests: Writeln(BmProgram.DebugInfo); }

          Renderer.BmGLSLAttribObjectSpaceToTangent[Parallax] :=
            BmProgram.CreateAttribute('object_space_to_tangent');

          BmProgram.Enable;
          BmProgram.SetUniform('light_position_world_space',
            Renderer.BumpMappingLightPosition);
          BmProgram.SetUniform('light_ambient_color',
            Renderer.BumpMappingLightAmbientColor);
          BmProgram.SetUniform('light_diffuse_color',
            Renderer.BumpMappingLightDiffuseColor);

          { set uniform samplers, so that fragment shader has access
            to all bound textures }
          BmProgram.SetUniform('tex_normal_map', 0);
          BmProgram.SetUniform('tex_original', 1);
          if Parallax then
            BmProgram.SetUniform('tex_height_map', 2);

          { TODO: this should restore previously bound program }
          BmProgram.Disable;

          Renderer.BmGLSLProgram[Parallax] := BmProgram;
        end;
    end;
  end;

var
  { We cannot store NormalMap, HeightMap references in TBumpMappingRenderer
    instance (although it's tempting, as it would be comfortable).
    Reason: they may be freed by UnprepareTexture calls, called from
    Unprepare method in TVRMLOpenGLRenderer (that may in turn be called
    by anything in the scene). Our NormalMap, HeightMap references would
    point then to invalid instances. }
  NormalMap, HeightMap: TGLTextureNode;
begin
  { don't bother loading textures for bump mapping if not enabled }
  if Renderer.BumpMappingMethod = bmNone then Exit;

  if IsImageTexture(Node.FdNormalMap.Value) then
    NormalMap := Renderer.GLTextureNodes.Prepare(State, TVRMLTextureNode(Node.FdNormalMap.Value), Renderer) else
    NormalMap := nil;

  if IsImageTexture(Node.FdHeightMap.Value) then
    HeightMap := Renderer.GLTextureNodes.Prepare(State, TVRMLTextureNode(Node.FdHeightMap.Value), Renderer) else
    HeightMap := nil;

      (* TODO: force texture to grayscale.

      { Calculate HeightMapGrayscale }
      { TODO: this is not nice to convert here, we should load
        straight to TGrayscalaImage }
      if OriginalTexture is TRGBImage then
        HeightMapGrayscale := TRGBImage(OriginalTexture).ToGrayscale else
      if OriginalTexture is TGrayscaleImage then
        HeightMapGrayscale := TGrayscaleImage(OriginalTexture) else
        HeightMapGrayscale := nil;

      if HeightMapGrayscale <> nil then
      try
        HeightMap := LoadGLTexture(HeightMapGrayscale,
            GL_LINEAR_MIPMAP_LINEAR, GL_LINEAR, TextureWrap);
        HeightMapScale := State.ParentShape.HeightMapScale;
      finally
        if HeightMapGrayscale <> OriginalTexture then
          FreeAndNil(HeightMapGrayscale);
      end;
      *)

  if IsGLImageTexture(NormalMap) then
  begin
    if IsGLImageTexture(HeightMap) and
       (Renderer.BumpMappingMethod >= bmGLSLParallax) then
      PrepareBumpMapping(true) else
      PrepareBumpMapping(false);
  end;
end;

procedure TBumpMappingRenderer.Unprepare;
begin
  if IsImageTexture(Node.FdNormalMap.Value) then
    Renderer.GLTextureNodes.Unprepare(TVRMLTextureNode(Node.FdNormalMap.Value));

  if IsImageTexture(Node.FdHeightMap.Value) then
    Renderer.GLTextureNodes.Unprepare(TVRMLTextureNode(Node.FdHeightMap.Value));
end;

function TBumpMappingRenderer.Enable(
  State: TVRMLGraphTraverseState;
  ColorTextureNode: TGLTextureNode): boolean;

  { Return OpenGL texture initialized for texture node Node.
    If Node is nil or invalid type,
    or it's corresponding GLNode (TGLTextureNode) is not initialized or
    invalid type, or OpenGL texture was not prepared for whatever reason
    --- returns 0. }
  function GLTexture(Node: TVRMLNode): TGLuint;
  var
    GLNode: TGLTextureNode;
  begin
    Result := 0;
    if IsImageTexture(Node) then
    begin
      GLNode := Renderer.GLTextureNodes.TextureNode(TVRMLTextureNode(Node));
      if IsGLImageTexture(GLNode) then
        Result := TGLImageTextureNode(GLNode).GLName;
    end;
  end;

var
  TexOriginal: TGLuint;
  TexOriginalAlpha: boolean;
  TexNormalMap: TGLuint;
  TexHeightMap: TGLuint;
  TexHeightMapScale: Single;
  MeshRenderer: TVRMLMeshRenderer;

  { pass alpha from primary_color (this is actual material Opacity) *
    texture alpha }
  procedure glTexEnvAlphaFromMaterialAndTexture;
  begin
    if TexOriginalAlpha then
    begin
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA_EXT, GL_MODULATE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_EXT, GL_PRIMARY_COLOR_EXT);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_EXT, GL_SRC_ALPHA);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_ALPHA_EXT, GL_TEXTURE);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_ALPHA_EXT, GL_SRC_ALPHA);
    end else
    begin
      glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_ALPHA_EXT, GL_REPLACE);
      glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_ALPHA_EXT, GL_PRIMARY_COLOR_EXT);
      glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_ALPHA_EXT, GL_SRC_ALPHA);
    end;
  end;

  procedure EnableForMultiTexDotNotNormalized;
  begin
    Renderer.ActiveTexture(0);
    glBindTexture(GL_TEXTURE_2D, TexNormalMap);
    glEnable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

    { texture unit 0: make dot between PRIMARY_COLOR and
      current texture (normal map). IOW, calculate diffuse factor. }
    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_DOT3_RGB_ARB);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_PRIMARY_COLOR_EXT);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_EXT, GL_TEXTURE);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_EXT, GL_SRC_COLOR);

    Renderer.ActiveTexture(1);
    glBindTexture(GL_TEXTURE_2D, TexOriginal);
    glEnable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

    { texture unit 1: multiply diffuse factor by texture value. }
    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_MODULATE);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_PREVIOUS_EXT);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_EXT, GL_TEXTURE);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_EXT, GL_SRC_COLOR);

    glTexEnvAlphaFromMaterialAndTexture;
  end;

  procedure EnableForMultiTexDotNormalized;
  begin
    { texture unit 0: just lookup your coordinates (which are
      3D lighting dir, not normalized) in cube map.
      This calculates normalized lighting dir. }
    Renderer.ActiveTexture(0);
    glBindTexture(GL_TEXTURE_CUBE_MAP_ARB, Renderer.TexNormalizationCube);
    glEnable(GL_TEXTURE_CUBE_MAP_ARB);
    { make sure GL_TEXTURE_2D is disabled here (other bump mapping
      methods could enable this) }
    glDisable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_REPLACE);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_TEXTURE);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);

    Renderer.ActiveTexture(1);
    glBindTexture(GL_TEXTURE_2D, TexNormalMap);
    glEnable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

    { texture unit 1: make dot between previous (light dir normalized) and
      current texture (normal map). IOW, calculate diffuse factor. }
    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_DOT3_RGB_ARB);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_PREVIOUS_EXT);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_EXT, GL_TEXTURE);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_EXT, GL_SRC_COLOR);

    Renderer.ActiveTexture(2);
    { We bind NormalAndBumpTex, although actually we don't use
      bump stored there as alpha channel (they are only for emboss
      method). }
    glBindTexture(GL_TEXTURE_2D, TexOriginal);
    glEnable(GL_TEXTURE_2D);
    glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_EXT);

    { texture unit 2: multiply diffuse factor by texture value. }
    glTexEnvi(GL_TEXTURE_ENV, GL_COMBINE_RGB_EXT, GL_MODULATE);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE0_RGB_EXT, GL_PREVIOUS_EXT);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND0_RGB_EXT, GL_SRC_COLOR);
    glTexEnvi(GL_TEXTURE_ENV, GL_SOURCE1_RGB_EXT, GL_TEXTURE);
    glTexEnvi(GL_TEXTURE_ENV, GL_OPERAND1_RGB_EXT, GL_SRC_COLOR);

    glTexEnvAlphaFromMaterialAndTexture;
  end;

  procedure EnableForGLSL;
  var
    BmParallax: boolean;
  begin
    { This binds textures in the same way as EnableForMultiTexDotNotNormalized,
      but this doesn't call glTexEnvi or glEnable(GL_TEXTURE_2D)
      (it's useless, shader will control this). }
    Renderer.ActiveTexture(0);
    glBindTexture(GL_TEXTURE_2D, TexNormalMap);

    Renderer.ActiveTexture(1);
    glBindTexture(GL_TEXTURE_2D, TexOriginal);

    BmParallax := MeshRenderer.BumpMappingMethod >= bmGLSLParallax;

    if BmParallax then
    begin
      Renderer.ActiveTexture(2);
      glBindTexture(GL_TEXTURE_2D, TexHeightMap);
    end;

    Renderer.BmGLSLProgram[BmParallax].Enable;
    Renderer.BmGLSLProgram[BmParallax].SetUniform('world_space_to_object',
      State.InvertedTransform);

    if BmParallax then
    begin
      Renderer.BmGLSLProgram[BmParallax].SetUniform('scale', TexHeightMapScale);
      if not Renderer.BmSteepParallaxMapping then
        Renderer.BmGLSLProgram[BmParallax].SetUniform('bias', -TexHeightMapScale/2);
    end;

    glMaterialv(GL_FRONT_AND_BACK, GL_AMBIENT, Vector4Single(1, 1, 1, 1));
  end;

begin
  Result := false;

  if not IsGLImageTexture(ColorTextureNode) then Exit;

  MeshRenderer := TVRMLMeshRenderer(Renderer.ExposedMeshRenderer);

  if (MeshRenderer <> nil) and
     MeshRenderer.BumpMappingAllowed and
     (Renderer.BumpMappingMethod <> bmNone) then
  begin
    TexNormalMap := GLTexture(Node.FdNormalMap.Value);
    if TexNormalMap <> 0 then
    begin
      MeshRenderer.BumpMappingMethod := Renderer.BumpMappingMethod;

      TexHeightMap := GLTexture(Node.FdHeightMap.Value);
      TexHeightMapScale := Node.FdHeightMapScale.Value;

      TexOriginal := TGLImageTextureNode(ColorTextureNode).GLName;
      TexOriginalAlpha := TGLImageTextureNode(ColorTextureNode).AlphaChannelType = atSimpleYesNo;

      { use parallax only if the model actually has heightMap }
      if (TexHeightMap = 0) and
         (MeshRenderer.BumpMappingMethod >= bmGLSLParallax) then
        MeshRenderer.BumpMappingMethod := bmGLSLNormal;
    end;

    Result := true;
  end;

  if Result and (MeshRenderer.BumpMappingMethod <> bmNone) then
  begin
    case MeshRenderer.BumpMappingMethod of
      bmMultiTexDotNotNormalized: EnableForMultiTexDotNotNormalized;
      bmMultiTexDotNormalized   : EnableForMultiTexDotNormalized;
      bmGLSLNormal,
      bmGLSLParallax            : EnableForGLSL;
      else raise EInternalError.Create('BumpMappingMethod ?');
    end;
  end;
end;

procedure TBumpMappingRenderer.Disable;
var
  MeshRenderer: TVRMLMeshRenderer;

  { DisableXxx procedures disable textures (2d and cube map)
    that were enabled by their SetXxx counterparts in RenderCoordinateBegin.
    In case of shader implementation, this also disables the shader.

    Note that they still leave some state behind:
    - currently bound textures
    - texture environments
    - the currently bound texture is left unspecified
    These must be set always when anything will enable some texture. }

  procedure DisableForMultiTexDotNotNormalized;
  begin
    Renderer.ActiveTexture(0);
    glDisable(GL_TEXTURE_2D);

    Renderer.ActiveTexture(1);
    glDisable(GL_TEXTURE_2D);
  end;

  procedure DisableForMultiTexDotNormalized;
  begin
    { texture unit 0: just lookup your coordinates (which are
      3D lighting dir, not normalized) in cube map.
      This calculates normalized lighting dir. }
    Renderer.ActiveTexture(0);
    glDisable(GL_TEXTURE_CUBE_MAP_ARB);

    Renderer.ActiveTexture(1);
    glDisable(GL_TEXTURE_2D);

    Renderer.ActiveTexture(2);
    glDisable(GL_TEXTURE_2D);;
  end;

  procedure DisableForGLSL;
  begin
    { No need to disable textures, as we didn't enable them in
      EnableForGLSL (shader doesn't need them enabled). }

    Renderer.BmGLSLProgram[MeshRenderer.BumpMappingMethod >= bmGLSLParallax].Disable;
  end;

begin
  MeshRenderer := TVRMLMeshRenderer(Renderer.ExposedMeshRenderer);

  if MeshRenderer.BumpMappingMethod <> bmNone then
  begin
    case MeshRenderer.BumpMappingMethod of
      bmMultiTexDotNotNormalized: DisableForMultiTexDotNotNormalized;
      bmMultiTexDotNormalized   : DisableForMultiTexDotNormalized;
      bmGLSLNormal,
      bmGLSLParallax            : DisableForGLSL;
      else raise EInternalError.Create('BumpMappingMethod ?');
    end;
  end;
end;

{ TBumpMappingRenderersList -------------------------------------------------- }

function TBumpMappingRenderersList.NodeIndex(ANode: TNodeKambiAppearance): Integer;
begin
  Result := inherited NodeIndex(ANode);
end;

function TBumpMappingRenderersList.Node(ANode: TNodeKambiAppearance): TBumpMappingRenderer;
begin
  Result := TBumpMappingRenderer(inherited Node(ANode));
end;

function TBumpMappingRenderersList.GetItems(const Index: Integer): TBumpMappingRenderer;
begin
  Result := TBumpMappingRenderer(inherited Items[Index]);
end;

procedure TBumpMappingRenderersList.Prepare(State: TVRMLGraphTraverseState;
  ARenderer: TVRMLOpenGLRenderer);
var
  ANode: TNodeKambiAppearance;
  BumpMappingRenderer: TBumpMappingRenderer;
begin
  if (State.ParentShape <> nil) and
     (State.ParentShape.FdAppearance.Value <> nil) and
     (State.ParentShape.FdAppearance.Value is TNodeKambiAppearance) then
  begin
    ANode := TNodeKambiAppearance(State.ParentShape.FdAppearance.Value);
    if Node(ANode) = nil then
    begin
      BumpMappingRenderer := TBumpMappingRenderer.Create(ARenderer, ANode);
      BumpMappingRenderer.Prepare(State);
      Add(BumpMappingRenderer);
    end;
  end;
end;

function TBumpMappingRenderersList.Enable(State: TVRMLGraphTraverseState;
  ColorTextureNode: TGLTextureNode): TBumpMappingRenderer;
var
  ANode: TNodeKambiAppearance;
begin
  Result := nil;

  if (State.ParentShape <> nil) and
     (State.ParentShape.FdAppearance.Value <> nil) and
     (State.ParentShape.FdAppearance.Value is TNodeKambiAppearance) then
  begin
    ANode := TNodeKambiAppearance(State.ParentShape.FdAppearance.Value);
    Result := Node(ANode);
    if Result <> nil then
    begin
      if not Result.Enable(State, ColorTextureNode) then
        Result := nil;
    end;
  end;
end;

{$endif read_implementation}
