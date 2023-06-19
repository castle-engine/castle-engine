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
  TResourceRendererList = class;

  { Resource connected to rendering (so connected to OpenGL context)
    of a given X3D node. }
  TResourceRenderer = class
  strict private
    NodeDestructionNotificationActive: Boolean;
    FRenderer: TGLRenderer;
    FNode: TX3DNode;
    FList: TResourceRendererList;
    procedure NodeDestructionNotification(const Node: TX3DNode);
  protected
    { Do the time-consuming preparations before using this node with renderer.

      Called always from our Prepare method. Our Prepare method takes care to catch
      common exceptions from this (EFramebufferError, ETextureLoadError),
      convert them to WritelnWarning and make Unprepare to undo the preparations.
      So Prepare-Unprepare work like constructor-destructor, with Unprepare
      having to be prepared to eventually finilize even incomplete instance. }
    procedure PrepareCore(const State: TX3DGraphTraverseState;
      const RenderOptions: TCastleRenderOptions); virtual; abstract;
  public
    constructor Create(const ARenderer: TGLRenderer;
      const AList: TResourceRendererList;
      const ANode: TX3DNode); virtual;
    destructor Destroy; override;

    property Renderer: TGLRenderer read FRenderer;

    { Reference to handled X3D node.
      Never @nil. }
    property Node: TX3DNode read FNode;

    { Do the time-consuming preparations before using this node with renderer.
      Calls PrepareCore. }
    procedure Prepare(const State: TX3DGraphTraverseState;
      const RenderOptions: TCastleRenderOptions);

    { Release all resources allocated by Prepare method.
      This is exposed so that you can override it, but do not call it directly
      (only TResourceRendererList.Unprepare should call it, and it will also remove
      the item from the list). }
    procedure Unprepare; virtual;
  end;

  { Manages resources connected to rendering (so connected to OpenGL context)
    that are associated with a given X3D node.

    This is a dictionary that should always be created with [doOwnsValues].
    Just Remove(Node) to unprepare OpenGL resources for a given node,
    destroying relevant TResourceRenderer.
    Just Clear to unprepare OpenGL resources for all nodes,
    destroying all TResourceRenderer instances here.
  }
  TResourceRendererList = class({$ifdef FPC}specialize{$endif}
    TObjectDictionary<TX3DNode, TResourceRenderer>)
  public
    { Looks for item with given Node.
      Returns @nil if not found. }
    function Node(ANode: TX3DNode): TResourceRenderer;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TResourceRenderer ---------------------------------------------------------- }

constructor TResourceRenderer.Create(const ARenderer: TGLRenderer;
  const AList: TResourceRendererList;
  const ANode: TX3DNode);
begin
  inherited Create;
  FRenderer := ARenderer;
  FList := AList;
  FNode := ANode;
end;

destructor TResourceRenderer.Destroy;
begin
  if NodeDestructionNotificationActive then
  begin
    Node.RemoveDestructionNotification({$ifdef FPC}@{$endif} NodeDestructionNotification);
    NodeDestructionNotificationActive := false;
  end;
  Unprepare;
  inherited;
end;

procedure TResourceRenderer.Prepare(const State: TX3DGraphTraverseState;
  const RenderOptions: TCastleRenderOptions);
begin
  try
    PrepareCore(State, RenderOptions);
    if not NodeDestructionNotificationActive then
    begin
      Node.AddDestructionNotification({$ifdef FPC}@{$endif} NodeDestructionNotification);
      NodeDestructionNotificationActive := true;
    end;
  except
    on E: EFramebufferError do
    begin
      Unprepare;
      WritelnWarning('VRML/X3D', 'Framebuffer error, generated texture not possible: '
        + E.Message);
    end;

    on E: ETextureLoadError do
    begin
      Unprepare;
      WritelnWarning('VRML/X3D', 'Cannot load texture to OpenGL: ' + E.Message);
    end;
  end;
end;

procedure TResourceRenderer.Unprepare;
begin
  if NodeDestructionNotificationActive then
  begin
    Node.RemoveDestructionNotification({$ifdef FPC}@{$endif} NodeDestructionNotification);
    NodeDestructionNotificationActive := false;
  end;
end;

procedure TResourceRenderer.NodeDestructionNotification(const Node: TX3DNode);
begin
  { Do not bother to RemoveDestructionNotification from our Unprepare/destructor,
    as Node is destroyed now. Doing RemoveDestructionNotification from destructor
    means that we call RemoveDestructionNotification on a node that it now iterating
    over FDestructionNotifications list (as it called this routine). }
  NodeDestructionNotificationActive := false;

  { In case a node gets destroyed, it's important to remove it from any caches.
    Otherwise, in case a new node with the same pointer will be allocated afterwards,
    it would be associated with (possibly incorrect) cache,
    e.g. GLTextureNodes.TextureNode would return something cached for a new texture.

    Testcase: in CGE editor, assign TCastleFont to TCastleText.CustomFont.
    Then change the TCastleFont (without touching anything else).
    Underneath, TCastleText will correctly cause freeing + recreation of the font TPixelTextureNode.
    But we also need to allocate new OpenGL resources for this node.

    Although TGLShape.Changed has "Renderer.UnprepareTexture(State.MainTexture)" call,
    but this is not the font texture.

    Calling there "Renderer.UnprepareTexture(State.FontTexture)" would be incorrect also,
    as at that point this may be the reference to a *new* texture (so, if it's different
    than the old texture, were freeing something else).
    It's most reliable to free OpenGL resource right when the corresponding node is also freed. }

  FList.Remove(Node);
  // do not do anything more here, as above caused our Unprepare + destruction
end;

{ TResourceRendererList ----------------------------------------------------- }

function TResourceRendererList.Node(ANode: TX3DNode): TResourceRenderer;
begin
  if not TryGetValue(ANode, Result) then
    Result := nil;
end;

{$endif}
