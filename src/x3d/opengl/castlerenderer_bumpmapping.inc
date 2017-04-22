{$ifdef read_interface}

  { Rendering with bump mapping.
    It's associated with an Appearance node (for our Appearance.normalMap extension)
    or CommonSurfaceShader node. }
  TBumpMappingRenderer = class(TResourceRenderer)
  private
    SurfaceShader: TCommonSurfaceShaderNode;
    Appearance: TAppearanceNode;
    function NormalTexture: TAbstractTexture2DNode;
    function DisplacementFactor: Single;
  public
    constructor Create(ARenderer: TGLRenderer; ANode: TX3DNode); override;

    { Prepare normalMap for rendering. }
    procedure Prepare(State: TX3DGraphTraverseState);

    { Release things initialized by Prepare. }
    procedure Unprepare; override;

    { Enable things for rendering with bump mapping. Returns success. }
    function Enable(var BoundTextureUnits: Cardinal;
      Shader: TShader): boolean;
  end;

  TBumpMappingRendererList = class(TResourceRendererList)
  private
    function GetItems(const Index: Integer): TBumpMappingRenderer;
  public
    { Looks for item with given Node.
      Returns @nil if not found. }
    function Node(ANode: TX3DNode): TBumpMappingRenderer;

    property Items[Index: Integer]: TBumpMappingRenderer read GetItems; default;

    { If this State contains appropriate TAppearanceNode,
      this will create and prepare TBumpMappingRenderer instance for it
      (if one doesn't exist already). }
    procedure Prepare(State: TX3DGraphTraverseState;
      ARenderer: TGLRenderer);

    { If this State contains appropriate TAppearanceNode,
      and TBumpMappingRenderer instance is prepared for this Node,
      we will call it's Enable method here.

      We return the TBumpMappingRenderer instance only if it existed
      and it's Enable method returned @true. Otherwise, we return @nil. }
    function Enable(State: TX3DGraphTraverseState;
      var BoundTextureUnits: Cardinal;
      Shader: TShader): TBumpMappingRenderer;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TBumpMappingRenderer.Create(ARenderer: TGLRenderer; ANode: TX3DNode);
begin
  inherited;
  if ANode is TCommonSurfaceShaderNode then
    SurfaceShader := TCommonSurfaceShaderNode(ANode)
  else
  if ANode is TAppearanceNode then
    Appearance := TAppearanceNode(ANode)
  else
    raise Exception.Create('Invalid node for TBumpMappingRenderer.Create');
end;

function TBumpMappingRenderer.NormalTexture: TAbstractTexture2DNode;
var
  PotentialResult: TX3DNode;
begin
  if SurfaceShader <> nil then
    PotentialResult := SurfaceShader.FdNormalTexture.Value
  else
    PotentialResult := Appearance.FdNormalMap.Value;
  if PotentialResult is TAbstractTexture2DNode then
    Result := TAbstractTexture2DNode(PotentialResult)
  else
    Result := nil;
end;

function TBumpMappingRenderer.DisplacementFactor: Single;
begin
  if SurfaceShader <> nil then
    Result := SurfaceShader.DisplacementFactor / 255.0
  else
    Result := Appearance.FdHeightMapScale.Value;
end;

procedure TBumpMappingRenderer.Prepare(State: TX3DGraphTraverseState);
var
  Tex: TAbstractTexture2DNode;
begin
  { don't bother loading textures for bump mapping if not enabled }
  if Renderer.BumpMapping = bmNone then Exit;

  { Note: We cannot store returned TGLTextureNode references in TBumpMappingRenderer
    instance (although it's tempting, as it would be comfortable).
    Reason: they may be freed by UnprepareTexture calls, called from
    Unprepare method in TGLRenderer (that may in turn be called
    by anything in the scene). Our references would then be invalid. }

  Tex := NormalTexture;
  if Tex <> nil then
    Renderer.GLTextureNodes.Prepare(State, Tex, Renderer);
end;

procedure TBumpMappingRenderer.Unprepare;
var
  Tex: TAbstractTexture2DNode;
begin
  Tex := NormalTexture;
  if Tex <> nil then
    Renderer.GLTextureNodes.Unprepare(Tex);
end;

function TBumpMappingRenderer.Enable(var BoundTextureUnits: Cardinal;
  Shader: TShader): boolean;

  { Return OpenGL texture initialized for texture node Node.
    If Node is nil,or it's corresponding GLNode (TGLTextureNode) is not initialized
    or invalid type, or OpenGL texture was not prepared for whatever reason
    --- returns 0. }
  function GLTexture(Node: TAbstractTexture2DNode; out DisplacementTextureInAlpha: boolean): TGLuint;
  var
    GLNode: TGLTextureNode;
  begin
    Result := 0;
    if Node <> nil then
    begin
      GLNode := Renderer.GLTextureNodes.TextureNode(TAbstractTexture2DNode(Node));
      if GLNode <> nil then
      begin
        if GLNode is TGLImageTextureNode then
          Result := TGLImageTextureNode(GLNode).GLName
        else
        if GLNode is TGLMovieTextureNode then
          Result := TGLMovieTextureNode(GLNode).GLName;

        DisplacementTextureInAlpha := TAbstractTextureNode(Node).AlphaChannelFinal <> acNone;
      end;
    end;
  end;

var
  Tex: TAbstractTexture2DNode;
  TexNormalMap: TGLuint;
  { Does NormalMap have alpha channel. }
  DisplacementTextureInAlpha: boolean;
begin
  Result := false;

  if Renderer.ShapeBumpMappingAllowed and
     (Renderer.BumpMapping <> bmNone) then
  begin
    Tex := NormalTexture;
    TexNormalMap := GLTexture(Tex, DisplacementTextureInAlpha);
    if TexNormalMap <> 0 then
    begin
      Result := true;
      Renderer.ShapeBumpMappingUsed := true;

      Renderer.ActiveTexture(BoundTextureUnits);
      glBindTexture(GL_TEXTURE_2D, TexNormalMap);
      Shader.EnableBumpMapping(Renderer.BumpMapping, BoundTextureUnits,
        DisplacementTextureInAlpha, DisplacementFactor);
      Inc(BoundTextureUnits);
    end;
  end;
end;

{ TBumpMappingRendererList -------------------------------------------------- }

function TBumpMappingRendererList.Node(ANode: TX3DNode): TBumpMappingRenderer;
begin
  Result := TBumpMappingRenderer(inherited Node(ANode));
end;

function TBumpMappingRendererList.GetItems(const Index: Integer): TBumpMappingRenderer;
begin
  Result := TBumpMappingRenderer(inherited Items[Index]);
end;

procedure TBumpMappingRendererList.Prepare(State: TX3DGraphTraverseState;
  ARenderer: TGLRenderer);

  procedure PrepareForNode(const ANode: TX3DNode);
  var
    BumpMappingRenderer: TBumpMappingRenderer;
  begin
    if Node(ANode) = nil then
    begin
      BumpMappingRenderer := TBumpMappingRenderer.Create(ARenderer, ANode);
      BumpMappingRenderer.Prepare(State);
      Add(BumpMappingRenderer);
    end;
  end;

var
  ShaderSurface: TCommonSurfaceShaderNode;
begin
  if State.ShapeNode <> nil then
  begin
    ShaderSurface := State.ShapeNode.CommonSurfaceShader;
    if (ShaderSurface <> nil) and
       (ShaderSurface.NormalTexture <> nil) then
      PrepareForNode(ShaderSurface)
    else
    if State.ShapeNode.Appearance <> nil then
      PrepareForNode(State.ShapeNode.Appearance);
  end;
end;

function TBumpMappingRendererList.Enable(State: TX3DGraphTraverseState;
  var BoundTextureUnits: Cardinal;
  Shader: TShader): TBumpMappingRenderer;

  procedure EnableForNode(const ANode: TX3DNode);
  begin
    Result := Node(ANode);
    if Result <> nil then
    begin
      if not Result.Enable(BoundTextureUnits, Shader) then
        Result := nil;
    end;
  end;

var
  ShaderSurface: TCommonSurfaceShaderNode;
begin
  if State.ShapeNode <> nil then
  begin
    ShaderSurface := State.ShapeNode.CommonSurfaceShader;
    if (ShaderSurface <> nil) and
       (ShaderSurface.NormalTexture <> nil) then
      EnableForNode(ShaderSurface)
    else
    if State.ShapeNode.Appearance <> nil then
      EnableForNode(State.ShapeNode.Appearance);
  end;
end;

{$endif read_implementation}
