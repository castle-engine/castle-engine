{$ifdef read_interface}

  { Rendering GLSL program. }
  TGLSLRenderer = class(TResourceRenderer)
  private
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer; ANode: TNodeComposedShader);

    function Node: TNodeComposedShader;

    { GLSLProgram prepared.

      @nil means that this GLSL program failed to initialize.
      So do not try to initialize it again, and no need to unprepare
      it from Cache (as Cache doesn't have this program).

      TODO: temp public }
    GLSLProgram: TGLSLProgram;

//TODO:    procedure Prepare(State: TVRMLGraphTraverseState);
    procedure Unprepare; override;
  end;

  TGLSLRenderersList = class(TResourceRenderersList)
  private
    function GetItems(const Index: Integer): TGLSLRenderer;
  public
    { Looks for item with given Node.
      Returns -1 if not found. }
    function NodeIndex(ANode: TNodeComposedShader): Integer;

    { Looks for item with given Node.
      Returns @nil if not found. }
    function Node(ANode: TNodeComposedShader): TGLSLRenderer;

    property Items[Index: Integer]: TGLSLRenderer read GetItems; default;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TGLSLRenderer.Create(ARenderer: TVRMLOpenGLRenderer;
  ANode: TNodeComposedShader);
begin
  inherited Create(ARenderer, ANode);
end;

function TGLSLRenderer.Node: TNodeComposedShader;
begin
  Result := TNodeComposedShader(inherited Node);
end;

procedure TGLSLRenderer.Unprepare;
begin
  if GLSLProgram <> nil then
    Renderer.Cache.GLSLProgram_DecReference(GLSLProgram);
end;

{ TGLSLRenderersList -------------------------------------------------- }

function TGLSLRenderersList.NodeIndex(ANode: TNodeComposedShader): Integer;
begin
  Result := inherited NodeIndex(ANode);
end;

function TGLSLRenderersList.Node(ANode: TNodeComposedShader): TGLSLRenderer;
begin
  Result := TGLSLRenderer(inherited Node(ANode));
end;

function TGLSLRenderersList.GetItems(const Index: Integer): TGLSLRenderer;
begin
  Result := TGLSLRenderer(inherited Items[Index]);
end;

{$endif read_implementation}
