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
      it from Cache (as Cache doesn't have this program). }
    GLSLProgram: TGLSLProgram;

    procedure Prepare;
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

    procedure Prepare(State: TVRMLGraphTraverseState;
      ARenderer: TVRMLOpenGLRenderer);

    { If some shader is available and prepared for this state,
      Enable it (and return it's program). Otherwise returns @nil. }
    function Enable(State: TVRMLGraphTraverseState): TGLSLProgram;
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

procedure TGLSLRenderer.Prepare;
begin
  try
    GLSLProgram := Renderer.Cache.GLSLProgram_IncReference(Node);
    Node.EventIsSelectedSend(true);
  except
    { EGLSLError catches errors from Cache.GLSLProgram_IncReference,
      including GLShaders errors like
      EGLSLShaderCompileError or EGLSLProgramLinkError }
    on E: EGLSLError do
    begin
      VRMLWarning(vwSerious, 'Error when initializing GLSL shader : ' + E.Message);
      GLSLProgram := nil;
      Node.EventIsSelectedSend(false);
    end;
  end;
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

procedure TGLSLRenderersList.Prepare(State: TVRMLGraphTraverseState;
  ARenderer: TVRMLOpenGLRenderer);
var
  I: Integer;
  ProgramNode: TNodeComposedShader;
  GLSLProgram: TGLSLProgram;
  GLSLRenderer: TGLSLRenderer;
begin
  { prepare GLSLProgram }
  if (not ARenderer.Attributes.PureGeometry) and
     ARenderer.Attributes.GLSLShaders and
     (State.ParentShape <> nil) and
     (State.ParentShape.Appearance <> nil) then
  begin
    for I := 0 to State.ParentShape.Appearance.FdShaders.Items.Count - 1 do
    begin
      ProgramNode := State.ParentShape.Appearance.GLSLShader(I);
      if ProgramNode <> nil then
      begin
        GLSLRenderer := Node(ProgramNode);

        if GLSLRenderer <> nil then
        begin
          { This ProgramNode was already prepared.
            So just take it's GLSLProgram (to decide lower whether we can
            Break or not now). }
          GLSLProgram := GLSLRenderer.GLSLProgram;
        end else
        begin
          GLSLRenderer := TGLSLRenderer.Create(ARenderer, ProgramNode);
          GLSLRenderer.Prepare;
          Add(GLSLRenderer);

          GLSLProgram := GLSLRenderer.GLSLProgram;
        end;

        { Only if successfull, break. }
        if GLSLProgram <> nil then
          Break;
      end else
      begin
        { GLSLShader(I) is nil, so this is not appropriate node class
          or "language" field was bad.
          So at least send him "isSelected" = false, if it's X3DShaderNode. }
        if State.ParentShape.Appearance.FdShaders.Items[I] is TNodeX3DShaderNode then
          (State.ParentShape.Appearance.FdShaders.Items[I] as TNodeX3DShaderNode).EventIsSelectedSend(false);
      end;
    end;
  end;
end;

function TGLSLRenderersList.Enable(State: TVRMLGraphTraverseState): TGLSLProgram;
var
  I: Integer;
  GLSLRenderer: TGLSLRenderer;
  ProgramNode: TNodeComposedShader;
begin
  Result := nil;
  if (State.ParentShape <> nil) and
     (State.ParentShape.Appearance <> nil) then
  begin
    for I := 0 to State.ParentShape.Appearance.FdShaders.Items.Count - 1 do
    begin
      ProgramNode := State.ParentShape.Appearance.GLSLShader(I);
      if ProgramNode <> nil then
      begin
        GLSLRenderer := Node(ProgramNode);
        if (GLSLRenderer <> nil) and
           (GLSLRenderer.GLSLProgram <> nil) then
        begin
          Result := GLSLRenderer.GLSLProgram;
          Result.Enable;
          Break;
        end;
      end;
    end;
  end;
end;

{$endif read_implementation}
