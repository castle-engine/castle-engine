{$ifdef read_interface}

  { Rendering with bump mapping. It's associated with Appearance node
    defining heightMap, normalMap and other bump mapping properties. }
  TBumpMappingRenderer = class(TResourceRenderer)
  private
    class function IsImageTexture(ANode: TVRMLNode): boolean;
    class function IsGLImageTexture(GLNode: TGLTextureNode): boolean;
  public
    { ANode must be TNodeAppearance }
    constructor Create(ARenderer: TVRMLGLRenderer; ANode: TVRMLNode); override;

    function Node: TNodeAppearance;

    { Prepare normalMap, heightMap (if exist) for rendering. }
    procedure Prepare(State: TVRMLGraphTraverseState);

    { Release things initialized by Prepare. }
    procedure Unprepare; override;

    { Enable things for rendering with bump mapping. Returns success. }
    function Enable(var BoundTextureUnits: Cardinal;
      Shader: TVRMLShader): boolean;
  end;

  TBumpMappingRenderersList = class(TResourceRenderersList)
  private
    function GetItems(const Index: Integer): TBumpMappingRenderer;
  public
    { Looks for item with given Node.
      Returns -1 if not found. }
    function NodeIndex(ANode: TNodeAppearance): Integer;

    { Looks for item with given Node.
      Returns @nil if not found. }
    function Node(ANode: TNodeAppearance): TBumpMappingRenderer;

    property Items[Index: Integer]: TBumpMappingRenderer read GetItems; default;

    { If this State contains appropriate TNodeAppearance,
      this will create and prepare TBumpMappingRenderer instance for it
      (if one doesn't exist already). }
    procedure Prepare(State: TVRMLGraphTraverseState;
      ARenderer: TVRMLGLRenderer);

    { If this State contains appropriate TNodeAppearance,
      and TBumpMappingRenderer instance is prepared for this Node,
      we will call it's Enable method here.

      We return the TBumpMappingRenderer instance only if it existed
      and it's Enable method returned @true. Otherwise, we return @nil. }
    function Enable(State: TVRMLGraphTraverseState;
      var BoundTextureUnits: Cardinal;
      Shader: TVRMLShader): TBumpMappingRenderer;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TBumpMappingRenderer.Create(ARenderer: TVRMLGLRenderer; ANode: TVRMLNode);
begin
  Assert(ANode is TNodeAppearance, 'TBumpMappingRenderer.Create acceps as Node only TNodeAppearance');
  inherited;
end;

function TBumpMappingRenderer.Node: TNodeAppearance;
begin
  Result := TNodeAppearance(inherited Node);
end;

class function TBumpMappingRenderer.IsImageTexture(ANode: TVRMLNode): boolean;
begin
  Result :=
    (ANode <> nil) and
    (ANode is TVRML2DTextureNode) and
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
begin
  { don't bother loading textures for bump mapping if not enabled }
  if not Renderer.BumpMapping then Exit;

  { Note: We cannot store returned TGLTextureNode references in TBumpMappingRenderer
    instance (although it's tempting, as it would be comfortable).
    Reason: they may be freed by UnprepareTexture calls, called from
    Unprepare method in TVRMLGLRenderer (that may in turn be called
    by anything in the scene). Our references would then be invalid. }

  if IsImageTexture(Node.FdNormalMap.Value) then
    Renderer.GLTextureNodes.Prepare(State, TVRML2DTextureNode(Node.FdNormalMap.Value), Renderer);

{ FdHeightMap not used now }
{ if IsImageTexture(Node.FdHeightMap.Value) then
    Renderer.GLTextureNodes.Prepare(State, TVRML2DTextureNode(Node.FdHeightMap.Value), Renderer); }
end;

procedure TBumpMappingRenderer.Unprepare;
begin
  if IsImageTexture(Node.FdNormalMap.Value) then
    Renderer.GLTextureNodes.Unprepare(TVRML2DTextureNode(Node.FdNormalMap.Value));

{ FdHeightMap not used now }
{ if IsImageTexture(Node.FdHeightMap.Value) then
    Renderer.GLTextureNodes.Unprepare(TVRML2DTextureNode(Node.FdHeightMap.Value)); }
end;

function TBumpMappingRenderer.Enable(var BoundTextureUnits: Cardinal;
  Shader: TVRMLShader): boolean;

  { Return OpenGL texture initialized for texture node Node.
    If Node is nil or invalid type,
    or it's corresponding GLNode (TGLTextureNode) is not initialized or
    invalid type, or OpenGL texture was not prepared for whatever reason
    --- returns 0. }
  function GLTexture(Node: TVRMLNode; out HeightMapInAlpha: boolean): TGLuint;
  var
    GLNode: TGLTextureNode;
    ImageGLNode: TGLImageTextureNode absolute GLNode;
  begin
    Result := 0;
    if IsImageTexture(Node) then
    begin
      GLNode := Renderer.GLTextureNodes.TextureNode(TVRML2DTextureNode(Node));
      if IsGLImageTexture(GLNode) then
      begin
        Result := ImageGLNode.GLName;
        HeightMapInAlpha := ImageGLNode.AlphaChannelType <> atNone;
      end;
    end;
  end;

var
  TexNormalMap: TGLuint;
  { Has NormalMap alpha channel. }
  HeightMapInAlpha: boolean;
begin
  Result := false;

  if Renderer.ShapeBumpMappingAllowed and
     Renderer.BumpMapping then
  begin
    TexNormalMap := GLTexture(Node.FdNormalMap.Value, HeightMapInAlpha);
    if TexNormalMap <> 0 then
    begin
      Result := true;
      if HeightMapInAlpha then
        Renderer.ShapeBumpMappingUsed := bmuParallax else
        Renderer.ShapeBumpMappingUsed := bmuNormal;

      Renderer.ActiveTexture(BoundTextureUnits);
      glBindTexture(GL_TEXTURE_2D, TexNormalMap);
      Shader.EnableBumpMapping(BoundTextureUnits, HeightMapInAlpha,
        Node.FdHeightMapScale.Value);
      Inc(BoundTextureUnits);
    end;
  end;
end;

{ TBumpMappingRenderersList -------------------------------------------------- }

function TBumpMappingRenderersList.NodeIndex(ANode: TNodeAppearance): Integer;
begin
  Result := inherited NodeIndex(ANode);
end;

function TBumpMappingRenderersList.Node(ANode: TNodeAppearance): TBumpMappingRenderer;
begin
  Result := TBumpMappingRenderer(inherited Node(ANode));
end;

function TBumpMappingRenderersList.GetItems(const Index: Integer): TBumpMappingRenderer;
begin
  Result := TBumpMappingRenderer(inherited Items[Index]);
end;

procedure TBumpMappingRenderersList.Prepare(State: TVRMLGraphTraverseState;
  ARenderer: TVRMLGLRenderer);
var
  ANode: TNodeAppearance;
  BumpMappingRenderer: TBumpMappingRenderer;
begin
  if (State.ShapeNode <> nil) and
     (State.ShapeNode.Appearance <> nil) then
  begin
    ANode := State.ShapeNode.Appearance;
    if Node(ANode) = nil then
    begin
      BumpMappingRenderer := TBumpMappingRenderer.Create(ARenderer, ANode);
      BumpMappingRenderer.Prepare(State);
      Add(BumpMappingRenderer);
    end;
  end;
end;

function TBumpMappingRenderersList.Enable(State: TVRMLGraphTraverseState;
  var BoundTextureUnits: Cardinal;
  Shader: TVRMLShader): TBumpMappingRenderer;
var
  ANode: TNodeAppearance;
begin
  Result := nil;

  if (State.ShapeNode <> nil) and
     (State.ShapeNode.Appearance <> nil) then
  begin
    ANode := State.ShapeNode.Appearance;
    Result := Node(ANode);
    if Result <> nil then
    begin
      if not Result.Enable(BoundTextureUnits, Shader) then
        Result := nil;
    end;
  end;
end;

{$endif read_implementation}
