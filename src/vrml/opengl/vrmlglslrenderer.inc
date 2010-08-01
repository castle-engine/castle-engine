{$ifdef read_interface}

  { Rendering GLSL program. }
  TGLSLRenderer = class(TResourceRenderer)
  private
    { GLSLProgram prepared.

      @nil means that this GLSL program failed to initialize.
      So do not try to initialize it again, and no need to unprepare
      it from Cache (as Cache doesn't have this program). }
    GLSLProgram: TGLSLProgram;
  public
    { ANode must be TNodeComposedShader }
    constructor Create(ARenderer: TVRMLOpenGLRenderer; ANode: TVRMLNode); override;

    function Node: TNodeComposedShader;

    procedure Prepare(State: TVRMLGraphTraverseState);
    procedure Unprepare; override;

    { Enable GLSL shader.
      Binds also texture nodes for it's uniform variables.
      (if any; returns BoundTextureUnits to indicate this).

      Returns @false on failure, which means that something (ComposedShader
      node, or texture node) was not successfully prepared (unable to load
      etc.). Note that some shader / texture could be bound anyway,
      but nothing should be enabled in case of failure.
      BoundTextureUnits is always 0 in case of failure. }
    function Enable(out BoundTextureUnits: Cardinal): boolean;

    { Disable things. Assumes that previously it did a successfull Enable. }
    procedure Disable;
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
      Enable it (and return it). Returns @nil if no such shader
      (that can be enabled successfully).

      BoundTextureUnits is always 0 if returns @nil. }
    function Enable(State: TVRMLGraphTraverseState;
      out BoundTextureUnits: Cardinal): TGLSLRenderer;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TGLSLRenderer.Create(ARenderer: TVRMLOpenGLRenderer; ANode: TVRMLNode);
begin
  Assert(ANode is TNodeComposedShader, 'TGLSLRenderer.Create acceps as Node only TNodeComposedShader');
  inherited;
end;

function TGLSLRenderer.Node: TNodeComposedShader;
begin
  Result := TNodeComposedShader(inherited Node);
end;

procedure TGLSLRenderer.Prepare(State: TVRMLGraphTraverseState);

  { Scan IDecls for SFNode and MFNode fields, prepare texture nodes inside. }
  procedure PrepareShaderFields(IDecls: TVRMLInterfaceDeclarationsList);

    { If TextureNode <> @nil and is a texture node, prepare it. }
    procedure PrepareTexture(TextureNode: TVRMLNode);
    begin
      if (TextureNode <> nil) and
         (TextureNode is TNodeX3DTextureNode) then
        Renderer.GLTextureNodes.Prepare(State,
          TNodeX3DTextureNode(TextureNode), Renderer);
    end;

  var
    I, J: Integer;
    UniformField: TVRMLField;
  begin
    for I := 0 to IDecls.Count - 1 do
    begin
      UniformField := IDecls.Items[I].Field;

      if UniformField <> nil then
      begin
        if UniformField is TSFNode then
        begin
          PrepareTexture(TSFNode(UniformField).Value);
        end else
        if UniformField is TMFNode then
        begin
          for J := 0 to TMFNode(UniformField).Count - 1 do
            PrepareTexture(TMFNode(UniformField).Items[J]);
        end;
      end;
    end;
  end;

begin
  try
    GLSLProgram := Renderer.Cache.GLSLProgram_IncReference(Node, Renderer.Attributes);
    Node.EventIsSelected.Send(true);

    { Only if GLSL program successfully created, prepare all texture nodes
      in Node.InterfaceDeclarations. These will be passed to uniform
      variable values in Enable. }
    PrepareShaderFields(Node.InterfaceDeclarations);
  except
    { EGLSLError catches errors from Cache.GLSLProgram_IncReference,
      including GLShaders errors like
      EGLSLShaderCompileError or EGLSLProgramLinkError }
    on E: EGLSLError do
    begin
      VRMLWarning(vwSerious, 'Error when initializing GLSL shader : ' + E.Message);
      GLSLProgram := nil;
      Node.EventIsSelected.Send(false);
    end;
  end;
end;

procedure TGLSLRenderer.Unprepare;

  { Scan IDecls for SFNode and MFNode fields, unprepare texture nodes inside. }
  procedure UnprepareShaderFields(IDecls: TVRMLInterfaceDeclarationsList);

    { If TextureNode <> @nil and is a texture node, prepare it. }
    procedure UnprepareTexture(TextureNode: TVRMLNode);
    begin
      if (TextureNode <> nil) and
         (TextureNode is TNodeX3DTextureNode) then
        Renderer.GLTextureNodes.Unprepare(TNodeX3DTextureNode(TextureNode));
    end;

  var
    I, J: Integer;
    UniformField: TVRMLField;
  begin
    for I := 0 to IDecls.Count - 1 do
    begin
      UniformField := IDecls.Items[I].Field;

      if UniformField <> nil then
      begin
        if UniformField is TSFNode then
        begin
          UnprepareTexture(TSFNode(UniformField).Value);
        end else
        if UniformField is TMFNode then
        begin
          for J := 0 to TMFNode(UniformField).Count - 1 do
            UnprepareTexture(TMFNode(UniformField).Items[J]);
        end;
      end;
    end;
  end;

begin
  if GLSLProgram <> nil then
  begin
    Renderer.Cache.GLSLProgram_DecReference(GLSLProgram);

    UnprepareShaderFields(Node.InterfaceDeclarations);
  end;
end;

function TGLSLRenderer.Enable(out BoundTextureUnits: Cardinal): boolean;

  { Scan IDecls for SFNode and MFNode fields, bind texture nodes inside. }
  procedure BindShaderFields(IDecls: TVRMLInterfaceDeclarationsList);

    { If TextureNode <> @nil and is a texture node, prepare it.
      Returns bound texture number (or -1 if failed).
      You should set shader uniform value to this number. }
    function BindTexture(TextureNode: TVRMLNode): LongInt;
    begin
      Result := -1;
      if (TextureNode <> nil) and
         (TextureNode is TNodeX3DTextureNode) then
      begin
        if BoundTextureUnits < Renderer.FreeGLTexturesCount then
        begin
          if Renderer.GLTextureNodes.Bind(
            TNodeX3DTextureNode(TextureNode), BoundTextureUnits) then
          begin
            Result := BoundTextureUnits;
            Inc(BoundTextureUnits);
          end else
            Enable := false;
        end else
          VRMLWarning(vwIgnorable, 'Not enough free texture units to bind all textures for this GLSL shader');
      end;
    end;

  var
    I, J: Integer;
    UniformField: TVRMLField;
    BoundValue: LongInt;
    UniformValue: TDynLongIntArray;
  begin
    for I := 0 to IDecls.Count - 1 do
    begin
      UniformField := IDecls.Items[I].Field;

      if UniformField <> nil then
      begin
        if UniformField is TSFNode then
        begin
          BoundValue := BindTexture(TSFNode(UniformField).Value);
          if BoundValue <> -1 then
            GLSLProgram.SetUniform(UniformField.Name, BoundValue);
        end else
        if UniformField is TMFNode then
        begin
          UniformValue := TDynLongIntArray.Create(TMFNode(UniformField).Count);
          try
            for J := 0 to TMFNode(UniformField).Count - 1 do
            begin
              BoundValue := BindTexture(TMFNode(UniformField).Items[J]);
              { We cannot just resign from setting the field entirely,
                and setting to -1 would likely produce wild OpenGL warnings
                (setting texture sampler to -1...).
                So on failure, assume safe 0. BindTexture already did
                necessary warnings for user. }
              if BoundValue = -1 then BoundValue := 0;
              UniformValue[J] := BoundValue;
            end;
            GLSLProgram.SetUniform(UniformField.Name, UniformValue);
          finally FreeAndNil(UniformValue) end;
        end;
      end;
    end;
  end;

begin
  BoundTextureUnits := 0;
  if (GLSLProgram <> nil) and
    { TODO: checking here Attributes.GLSLShaders should not be needed,
      as TGLSLRenderersList.Prepare makes sure GLSL shaders should
      not be even prepared when Attributes.GLSLShaders is false.
      However, for our trick for VSM to remove BeforeChange
      from SetGLSLShaders, this is needed. }
    Renderer.Attributes.GLSLShaders then
  begin
    Result := true;
    GLSLProgram.Enable;
    BindShaderFields(Node.InterfaceDeclarations);
  end else
    Result := false;
end;

procedure TGLSLRenderer.Disable;
begin
  GLSLProgram.Disable;
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
     (State.ShapeNode <> nil) and
     (State.ShapeNode.Appearance <> nil) then
  begin
    for I := 0 to State.ShapeNode.Appearance.FdShaders.Items.Count - 1 do
    begin
      ProgramNode := State.ShapeNode.Appearance.GLSLShader(I);
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
          GLSLRenderer.Prepare(State);
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
        if State.ShapeNode.Appearance.FdShaders.Items[I] is TNodeX3DShaderNode then
          (State.ShapeNode.Appearance.FdShaders.Items[I] as TNodeX3DShaderNode).EventIsSelected.Send(false);
      end;
    end;
  end;
end;

function TGLSLRenderersList.Enable(State: TVRMLGraphTraverseState;
  out BoundTextureUnits: Cardinal): TGLSLRenderer;
var
  I: Integer;
  ProgramNode: TNodeComposedShader;
begin
  Result := nil;
  if (State.ShapeNode <> nil) and
     (State.ShapeNode.Appearance <> nil) then
  begin
    for I := 0 to State.ShapeNode.Appearance.FdShaders.Items.Count - 1 do
    begin
      ProgramNode := State.ShapeNode.Appearance.GLSLShader(I);
      if ProgramNode <> nil then
      begin
        Result := Node(ProgramNode);
        if Result <> nil then
        begin
          if Result.Enable(BoundTextureUnits) then
            Break else
            Result := nil;
        end;
      end;
    end;
  end;

  { if no GLSL shader enabled fully successfully, ignore if some shader
    bound some texture units. }
  if Result = nil then
    BoundTextureUnits := 0;
end;

{$endif read_implementation}
