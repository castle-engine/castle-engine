{$ifdef read_interface}

  { Rendering GLSL program. }
  TGLSLRenderer = class(TResourceRenderer)
  private
    { GLSLProgram prepared.

      @nil means that this GLSL program failed to initialize.
      So do not try to initialize it again, and no need to unprepare
      it from Cache (as Cache doesn't have this program). }
    GLSLProgram: TVRMLGLSLProgram;
  public
    { ANode must be TNodeComposedShader }
    constructor Create(ARenderer: TVRMLGLRenderer; ANode: TVRMLNode); override;

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

    { Prepare GLSL shader for a "shaders" list. In case of the overloaded
      version without the Shaders parameter, we just prepare shaders
      for current Shape (inside State.ShapeNode.Appearance.FdShaders field).

      If any suitable ComposedShader node will be found, we will create
      and prepare TGLSLRenderer associated with it and add it to the list.

      We return under ShaderProgram the GLSL program created
      (or aquired from cache, if the ComposedShader node was already prepared).
      May be @nil if no ComposedShader node found (so no associated TGLSLRenderer),
      or associated TGLSLRenderer exists but it's GLSLProgram is @nil
      (which means that program failed to compile / link).

      @groupBegin }
    procedure Prepare(
      State: TVRMLGraphTraverseState;  ARenderer: TVRMLGLRenderer;
      Shaders: TMFNodeShaders; out GLSLProgram: TGLSLProgram);
    procedure Prepare(
      State: TVRMLGraphTraverseState; ARenderer: TVRMLGLRenderer);
    { @groupEnd }

    { If some shader is available and prepared for this state,
      Enable it (and return it). Returns @nil if no such shader
      (that can be enabled successfully).

      BoundTextureUnits is always 0 if returns @nil. }
    function Enable(State: TVRMLGraphTraverseState;
      out BoundTextureUnits: Cardinal): TGLSLRenderer;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TVRMLGLSLProgram ----------------------------------------------------------- }

function TVRMLGLSLProgram.SetupUniforms(var BoundTextureUnits: Cardinal): boolean;

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
        if BoundTextureUnits < GLMaxTextureUnits then
        begin
          if Renderer.GLTextureNodes.Bind(
            TNodeX3DTextureNode(TextureNode), BoundTextureUnits) then
          begin
            Result := BoundTextureUnits;
            Inc(BoundTextureUnits);
          end else
            SetupUniforms := false;
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
            SetUniform(UniformField.Name, BoundValue);
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
            SetUniform(UniformField.Name, UniformValue);
          finally FreeAndNil(UniformValue) end;
        end;
      end;
    end;
  end;

begin
  Result := inherited SetupUniforms(BoundTextureUnits);
  BindShaderFields(Node.InterfaceDeclarations);
end;

{ TGLSLRenderer -------------------------------------------------------------- }

constructor TGLSLRenderer.Create(ARenderer: TVRMLGLRenderer; ANode: TVRMLNode);
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
  GLSLProgram := Renderer.Cache.GLSLProgram_IncReference(Node, Renderer.Attributes);

  if GLSLProgram <> nil then
  begin
    GLSLProgram.Renderer := Renderer;
    GLSLProgram.Node := Node;

    { Only if GLSL program successfully created, prepare all texture nodes
      in Node.InterfaceDeclarations. These will be passed to uniform
      variable values in Enable. }
    PrepareShaderFields(Node.InterfaceDeclarations);
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
begin
  BoundTextureUnits := 0;
  if (GLSLProgram <> nil) and
    { Check here for Attributes.GLSLShaders, not at TGLSLRenderersList.Prepare.
      This way we can quickly change Attributes.GLSLShaders value at runtime
      (this is even used for VarianceShadowMaps, see TVRMLGLScene.Render
      RestoreGLSLShaders trick), no need to prepare again everything. }
    Renderer.Attributes.GLSLShaders then
  begin
    GLSLProgram.Enable;
    Result := GLSLProgram.SetupUniforms(BoundTextureUnits);
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
  ARenderer: TVRMLGLRenderer);
var
  IgnoreGLSLProgram: TGLSLProgram;
begin
  if (State.ShapeNode <> nil) and
     (State.ShapeNode.Appearance <> nil) then
    Prepare(State, ARenderer, State.ShapeNode.Appearance.FdShaders,
      IgnoreGLSLProgram);
end;

procedure TGLSLRenderersList.Prepare(State: TVRMLGraphTraverseState;
  ARenderer: TVRMLGLRenderer; Shaders: TMFNodeShaders;
  out GLSLProgram: TGLSLProgram);
var
  I: Integer;
  ProgramNode: TNodeComposedShader;
  GLSLRenderer: TGLSLRenderer;
begin
  GLSLProgram := nil;
  if not ARenderer.Attributes.PureGeometry then
  begin
    for I := 0 to Shaders.Count - 1 do
    begin
      ProgramNode := Shaders.GLSLShader(I);
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
        if Shaders.Items[I] is TNodeX3DShaderNode then
          (Shaders.Items[I] as TNodeX3DShaderNode).EventIsSelected.Send(false);
      end;
    end;
  end;
end;

function TGLSLRenderersList.Enable(State: TVRMLGraphTraverseState;
  out BoundTextureUnits: Cardinal): TGLSLRenderer;
var
  I: Integer;
  ProgramNode: TNodeComposedShader;
  Shaders: TMFNodeShaders;
begin
  Result := nil;
  if (State.ShapeNode <> nil) and
     (State.ShapeNode.Appearance <> nil) then
  begin
    Shaders := State.ShapeNode.Appearance.FdShaders;
    for I := 0 to Shaders.Count - 1 do
    begin
      ProgramNode := Shaders.GLSLShader(I);
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
