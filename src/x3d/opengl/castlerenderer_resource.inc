{$ifdef read_interface}

  { Base class for TGLRenderer helper. Such helper is associated
    with a given VRML node, and has methods like Prepare, Enable
    (their exact semantics depends on descendant) and Unprepare. }
  TResourceRenderer = class
  private
    FRenderer: TGLRenderer;
    FNode: TX3DNode;
  public
    constructor Create(ARenderer: TGLRenderer; ANode: TX3DNode); virtual;

    property Renderer: TGLRenderer read FRenderer;

    { Reference to handled VRML node.
      Never @nil. }
    property Node: TX3DNode read FNode;

    { Release all resources allocated by Prepare method. }
    procedure Unprepare; virtual; abstract;
  end;

  TResourceRendererList = class(specialize TFPGObjectList<TResourceRenderer>)
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

    { Unprepare and free and nodes. Clears the list. }
    procedure UnprepareAll;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TResourceRenderer ---------------------------------------------------------- }

constructor TResourceRenderer.Create(ARenderer: TGLRenderer;
  ANode: TX3DNode);
begin
  inherited Create;
  FRenderer := ARenderer;
  FNode := ANode;
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
