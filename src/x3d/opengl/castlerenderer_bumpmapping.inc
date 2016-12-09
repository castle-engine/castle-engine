{$ifdef read_interface}

  { Rendering with bump mapping. It's associated with Appearance node
    defining heightMap, normalMap and other bump mapping properties. }
  TBumpMappingRenderer = class(TResourceRenderer)
  private
    class function IsImageTexture(ANode: TX3DNode): boolean;
  public
    { ANode must be TAppearanceNode }
    constructor Create(ARenderer: TGLRenderer; ANode: TX3DNode); override;

    function Node: TAppearanceNode;

    { Prepare normalMap, heightMap (if exist) for rendering. }
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
      Returns -1 if not found. }
    function NodeIndex(ANode: TAppearanceNode): Integer;

    { Looks for item with given Node.
      Returns @nil if not found. }
    function Node(ANode: TAppearanceNode): TBumpMappingRenderer;

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
  Assert(ANode is TAppearanceNode, 'TBumpMappingRenderer.Create acceps as Node only TAppearanceNode');
  inherited;
end;

function TBumpMappingRenderer.Node: TAppearanceNode;
begin
  Result := TAppearanceNode(inherited Node);
end;

class function TBumpMappingRenderer.IsImageTexture(ANode: TX3DNode): boolean;
begin
  Result :=
    (ANode <> nil) and
    (ANode is TAbstractTexture2DNode);
end;

procedure TBumpMappingRenderer.Prepare(State: TX3DGraphTraverseState);
begin
  { don't bother loading textures for bump mapping if not enabled }
  if Renderer.BumpMapping = bmNone then Exit;

  { Note: We cannot store returned TGLTextureNode references in TBumpMappingRenderer
    instance (although it's tempting, as it would be comfortable).
    Reason: they may be freed by UnprepareTexture calls, called from
    Unprepare method in TGLRenderer (that may in turn be called
    by anything in the scene). Our references would then be invalid. }

  if IsImageTexture(Node.FdNormalMap.Value) then
    Renderer.GLTextureNodes.Prepare(State, TAbstractTexture2DNode(Node.FdNormalMap.Value), Renderer);

{ FdHeightMap not used now }
{ if IsImageTexture(Node.FdHeightMap.Value) then
    Renderer.GLTextureNodes.Prepare(State, TAbstractTexture2DNode(Node.FdHeightMap.Value), Renderer); }
end;

procedure TBumpMappingRenderer.Unprepare;
begin
  if IsImageTexture(Node.FdNormalMap.Value) then
    Renderer.GLTextureNodes.Unprepare(TAbstractTexture2DNode(Node.FdNormalMap.Value));

{ FdHeightMap not used now }
{ if IsImageTexture(Node.FdHeightMap.Value) then
    Renderer.GLTextureNodes.Unprepare(TAbstractTexture2DNode(Node.FdHeightMap.Value)); }
end;

function TBumpMappingRenderer.Enable(var BoundTextureUnits: Cardinal;
  Shader: TShader): boolean;

  { Return OpenGL texture initialized for texture node Node.
    If Node is nil or invalid type,
    or it's corresponding GLNode (TGLTextureNode) is not initialized or
    invalid type, or OpenGL texture was not prepared for whatever reason
    --- returns 0. }
  function GLTexture(Node: TX3DNode; out HeightMapInAlpha: boolean): TGLuint;
  var
    GLNode: TGLTextureNode;
  begin
    Result := 0;
    if IsImageTexture(Node) then
    begin
      GLNode := Renderer.GLTextureNodes.TextureNode(TAbstractTexture2DNode(Node));
      if GLNode <> nil then
      begin
        if GLNode is TGLImageTextureNode then
          Result := TGLImageTextureNode(GLNode).GLName
        else
        if GLNode is TGLMovieTextureNode then
          Result := TGLMovieTextureNode(GLNode).GLName;

        HeightMapInAlpha := TAbstractTextureNode(Node).AlphaChannelFinal <> acNone;
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
     (Renderer.BumpMapping <> bmNone) then
  begin
    TexNormalMap := GLTexture(Node.FdNormalMap.Value, HeightMapInAlpha);
    if TexNormalMap <> 0 then
    begin
      Result := true;
      Renderer.ShapeBumpMappingUsed := true;

      Renderer.ActiveTexture(BoundTextureUnits);
      glBindTexture(GL_TEXTURE_2D, TexNormalMap);
      Shader.EnableBumpMapping(Renderer.BumpMapping, BoundTextureUnits,
        HeightMapInAlpha, Node.FdHeightMapScale.Value);
      Inc(BoundTextureUnits);
    end;
  end;
end;

{ TBumpMappingRendererList -------------------------------------------------- }

function TBumpMappingRendererList.NodeIndex(ANode: TAppearanceNode): Integer;
begin
  Result := inherited NodeIndex(ANode);
end;

function TBumpMappingRendererList.Node(ANode: TAppearanceNode): TBumpMappingRenderer;
begin
  Result := TBumpMappingRenderer(inherited Node(ANode));
end;

function TBumpMappingRendererList.GetItems(const Index: Integer): TBumpMappingRenderer;
begin
  Result := TBumpMappingRenderer(inherited Items[Index]);
end;

procedure TBumpMappingRendererList.Prepare(State: TX3DGraphTraverseState;
  ARenderer: TGLRenderer);
var
  ANode: TAppearanceNode;
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

function TBumpMappingRendererList.Enable(State: TX3DGraphTraverseState;
  var BoundTextureUnits: Cardinal;
  Shader: TShader): TBumpMappingRenderer;
var
  ANode: TAppearanceNode;
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
