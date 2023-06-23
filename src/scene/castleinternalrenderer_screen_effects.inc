{%MainUnit castleinternalrenderer.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
//type
  { Rendering resource for TScreenEffectNode. }
  TScreenEffectResource = class(TRendererResource)
  strict private
    { Did we try to initialize ShaderProgram, i.e. was PrepareCore called.

      We guarantee "ShaderLoaded=false => ShaderProgram=nil".

      So when ShaderLoaded=true, it may be be that ShaderProgram<>nil
      (if shader compiled OK) or ShaderProgram=nil (if shader compilation
      failed). }
    ShaderLoaded: Boolean;
    FNode: TScreenEffectNode;
  protected
    procedure PrepareCore(const RenderOptions: TCastleRenderOptions); override;
    procedure UnprepareCore; override;
  public
    { GLSL shader for the ScreenEffect node.
      @nil if not prepared, or failed to be prepared (e.g. shader didn't compile). }
    ShaderProgram: TX3DGLSLProgram;

    constructor Create(const ANode: TX3DNode); override;

    { Associated node.
      This obscures parent Node property,
      which is OK -- in this class Node is always guaranteed to be
      TScreenEffectNode. }
    property Node: TScreenEffectNode read FNode;
  end;

  { Manage TScreenEffectResource connected to X3D nodes.

    Non-instantiable class (has only class methods).

    This should be the only code accessing
    TScreenEffectNode.InternalRendererResource field,
    everything else should go through this class. }
  TScreenEffectResources = class abstract
  public
    { Prepare renderer resources for given node. }
    class procedure Prepare(
      const RenderOptions: TCastleRenderOptions;
      const Node: TScreenEffectNode);

    { Renderer resource associated with given TScreenEffectNode.
      Returns @nil if no resource is associated yet (call Prepare to initialize it). }
    class function Get(const Node: TScreenEffectNode): TScreenEffectResource;
  end;
{$endif read_interface}

{$ifdef read_implementation}

{ TScreenEffectResources ---------------------------------------------------- }

class procedure TScreenEffectResources.Prepare(const RenderOptions: TCastleRenderOptions;
  const Node: TScreenEffectNode);
var
  RendererResource: TScreenEffectResource;
begin
  { Unlike TTextureResources.Prepare, we don't allow nil argument.
    This is more consistent in CGE, see
    https://castle-engine.io/coding_conventions#nil }
  Assert(Node <> nil);
  if Node.InternalRendererResource = nil then
  begin
    RendererResource := TScreenEffectResource.Create(Node);
    RendererResource.Prepare(RenderOptions);
    Node.InternalRendererResource := RendererResource;
  end;
end;

class function TScreenEffectResources.Get(const Node: TScreenEffectNode):
  TScreenEffectResource;
begin
  Result := Node.InternalRendererResource as TScreenEffectResource;
end;

{ TScreenEffectResource ----------------------------------------------------- }

procedure TScreenEffectResource.PrepareCore(const RenderOptions: TCastleRenderOptions);

  procedure PrepareIDecls(const IDecls: TX3DInterfaceDeclarationList);

    { If TextureNode <> @nil and is a texture node, prepare it. }
    procedure PrepareTexture(TextureNode: TX3DNode);
    begin
      if (TextureNode <> nil) and
         (TextureNode is TAbstractTextureNode) then
        TTextureResources.Prepare(RenderOptions, TAbstractTextureNode(TextureNode));
    end;

  var
    I, J: Integer;
    UniformField: TX3DField;
  begin
    if IDecls = nil then Exit; { ignore nodes without interface declararations }

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
            PrepareTexture(TMFNode(UniformField)[J]);
        end;
      end;
    end;
  end;

  { Prepare all textures within X3D "interface declarations" of the given Nodes. }
  procedure PrepareIDeclsList(Nodes: TMFNode);
  var
    I: Integer;
  begin
    for I := 0 to Nodes.Count - 1 do
      PrepareIDecls(Nodes[I].InterfaceDeclarations);
  end;

var
  Shader: TShader;
  ShaderNode: TComposedShaderNode;
  DummyCamera: TRenderingCamera;
begin
  if not ShaderLoaded then
  begin
    Assert(ShaderProgram = nil);
    ShaderLoaded := true;

    { Note: Strictly speaking, we should not need to check
      "not GLFeatures.EnableFixedFunction" below.
      Without this check, TX3DGLSLProgram.Create below will raise an exception
      which will cause to abort.

      But:
      - This is a bit inefficient way to abort.
      - It causes unknown issues: view3dscene --debug-fixed-function,
        enable any (like "visualize depth") screen effect,
        and you may experience crash in TCastleScene.ScreenEffectsCount
        as if ScreenEffectNodes was nil.

        Not debugged to the end:
        situation seems impossible and in debugger mysteriously
        doesn't occur. And logging inconclusive:

          WritelnLog('TCastleScene.ScreenEffectsCount %s', [BoolToStr(ScreenEffectNodes <> nil, true)]);

        sometimes shown we indeed have nil right before crash,
        but sometimes not...
    }

    if Node.FdEnabled.Value and (not GLFeatures.EnableFixedFunction) then
    begin
      { make sure that textures inside shaders are prepared }
      PrepareIDeclsList(Node.FdShaders);

      Shader := TShader.Create;
      try
        DummyCamera := TRenderingCamera.Create;
        try
          Shader.RenderingCamera := DummyCamera;

          { for ScreenEffect, we require that some ComposedShader was present.
            Rendering with default TShader shader makes no sense. }
          if Shader.EnableCustomShaderCode(Node.FdShaders, ShaderNode) then
          try
            ShaderProgram := TX3DGLSLProgram.Create;
            Shader.AddScreenEffectCode(Node.FdNeedsDepth.Value);
            Shader.LinkProgram(ShaderProgram, Node.NiceName);

            { We have to ignore invalid uniforms, as it's normal that when
              rendering screen effect we will pass some screen_* variables
              that you will not use. }
            ShaderProgram.UniformMissing := umIgnore;
          except on E: EGLSLError do
            begin
              FreeAndNil(ShaderProgram);
              WritelnWarning('Renderer', Format('Cannot use GLSL shader for ScreenEffect: %s',
                [E.Message]));
            end;
          end;
        finally FreeAndNil(DummyCamera) end;
      finally FreeAndNil(Shader) end;
    end;
  end;
end;

procedure TScreenEffectResource.UnprepareCore;
begin
  FreeAndNil(ShaderProgram);
  ShaderLoaded := false;
end;

constructor TScreenEffectResource.Create(const ANode: TX3DNode);
begin
  inherited;
  if not (ANode is TScreenEffectNode) then
    raise EInternalError.Create('TScreenEffectResource.Create: ANode must be TScreenEffectNode');
  FNode := TScreenEffectNode(ANode);
end;

{$endif read_implementation}