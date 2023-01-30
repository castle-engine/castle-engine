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

  { Base class for TGLRenderer helper. Such helper is associated
    with a given X3D node, and has methods like Prepare, Enable
    (their exact semantics depends on descendant) and Unprepare. }
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
    procedure PrepareCore(State: TX3DGraphTraverseState); virtual; abstract;
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
    procedure Prepare(State: TX3DGraphTraverseState);

    { Release all resources allocated by Prepare method.
      This is exposed so that you can override it, but do not call it directly
      (only TResourceRendererList.Unprepare should call it, and it will also remove
      the item from the list). }
    procedure Unprepare; virtual;
  end;

  TResourceRendererList = class({$ifdef FPC}specialize{$endif} TObjectList<TResourceRenderer>)
  private
    InsideUnprepareAll: Cardinal;
  public
    { Looks for item with given Node.
      Returns -1 if not found. }
    function NodeIndex(ANode: TX3DNode): Integer;

    { Looks for item with given Node.
      Returns @nil if not found. }
    function Node(ANode: TX3DNode): TResourceRenderer;

    { If resources for this Node were created (and possibly prepared),
      unprepare and remove them.

      Note: for complicated reasons, this is ignored if called during
      UnprepareAll call on the same list. E.g. it may happen when
      your TResourceRenderer calls Unprepare on it's own list from it's
      own Unprepare (like TGLMultiTextureNode). Although it's ignored
      in this case, you still can be sure ANode will be unprepared (as UnprepareAll
      will eventually unprepare, well, all). Just be aware that effect may
      be delayed in this case. }
    procedure Unprepare(ANode: TX3DNode);

    { Unprepare and free all nodes. Clears the list. }
    procedure UnprepareAll;
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
  inherited;
end;

procedure TResourceRenderer.Prepare(State: TX3DGraphTraverseState);
begin
  try
    PrepareCore(State);
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

  FList.Unprepare(Node);
  // do not do anything more here, as this object was freed by the above Unprepare call
end;

{ TResourceRendererList ----------------------------------------------------- }

function TResourceRendererList.NodeIndex(ANode: TX3DNode): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Node = ANode then Exit;
  Result := -1;
end;

function TResourceRendererList.Node(ANode: TX3DNode): TResourceRenderer;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.Node = ANode then Exit;
  end;
  Result := nil;
end;

procedure TResourceRendererList.Unprepare(ANode: TX3DNode);
var
  I: integer;
begin
  { If we're already inside UnprepareAll on the same instance, prevent
    Unprepare call.

    This may happen if some TResourceRenderer overrides
    Unprepare, and calls Unprepare *on the same list it's on* in it's own
    Unprepare. For example, TGLMultiTextureNode.Unprepare unprepares also
    it's own items.

    In this case, the list is half-destroyed (some items freed, so invalid
    pointers), so Unprepare cannot work correctly on it. Moreover, it would
    actually break UnprepareAll if Unprepare would modify the list contents
    (UnprepareAll loops over the list, assuming nothing else will change the
    list during this time.) Solution --- simply ignore Unprepare in this case.
    It's safe --- we know that all items on the list will eventually get
    unprepared. }
  if InsideUnprepareAll > 0 then Exit;

  { Call Unprepare and release related instance. }

  I := NodeIndex(ANode);
  if I >= 0 then
  begin
    Items[I].Unprepare;
    { After Unprepare on a MultiTexture node,
      a node could change it's index, because Unprepare on a MultiTexture
      node calls this recursively for it's children. So MultiTexture children
      are unprepared and removed from this list, causing the MultiTexture node
      itself to change it's position on a list.
      It's safest to just query it again. }
    I := NodeIndex(ANode);
    Assert(I <> -1, 'After Unprepare, node no longer available on a texture list');
    Items[I].Free;
    Delete(I);
  end;
end;

procedure TResourceRendererList.UnprepareAll;
var
  I: Integer;
begin
  Inc(InsideUnprepareAll);
  try
    for I := 0 to Count - 1 do
    begin
      Items[I].Unprepare;
      Items[I].Free;
    end;
    Count := 0;
  finally Dec(InsideUnprepareAll) end;
end;

{$endif}
