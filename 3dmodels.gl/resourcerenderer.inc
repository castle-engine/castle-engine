{$ifdef read_interface}

  { Base class for TVRMLOpenGLRenderer helper. Such helper is associated
    with a given VRML node, and has methods like Prepare, Enable
    (their exact semantics depends on descendant) and Unprepare. }
  TResourceRenderer = class
  private
    FRenderer: TVRMLOpenGLRenderer;
    FNode: TVRMLNode;
  public
    constructor Create(ARenderer: TVRMLOpenGLRenderer; ANode: TVRMLNode);

    property Renderer: TVRMLOpenGLRenderer read FRenderer;

    { Reference to handled VRML node.
      Never @nil. }
    property Node: TVRMLNode read FNode;

    { Release all resources allocated by Prepare method. }
    procedure Unprepare; virtual; abstract;
  end;

  TObjectsListItem_1 = TResourceRenderer;
  {$I objectslist_1.inc}
  TResourceRenderersList = class(TObjectsList_1)
  public
    { Looks for item with given Node.
      Returns -1 if not found. }
    function NodeIndex(ANode: TVRMLNode): Integer;

    { Looks for item with given Node.
      Returns @nil if not found. }
    function Node(ANode: TVRMLNode): TResourceRenderer;

    { If resources for this Node were created (and possibly prepared),
      unprepare and remove them. }
    procedure Unprepare(ANode: TVRMLNode);

    { Unprepare and free and nodes. Clears the list. }
    procedure UnprepareAll;
  end;

{$endif read_interface}

{$ifdef read_implementation}
{$I objectslist_1.inc}

{ TResourceRenderer ---------------------------------------------------------- }

constructor TResourceRenderer.Create(ARenderer: TVRMLOpenGLRenderer;
  ANode: TVRMLNode);
begin
  inherited Create;
  FRenderer := ARenderer;
  FNode := ANode;
end;

{ TResourceRenderersList ----------------------------------------------------- }

function TResourceRenderersList.NodeIndex(ANode: TVRMLNode): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Node = ANode then Exit;
  Result := -1;
end;

function TResourceRenderersList.Node(ANode: TVRMLNode): TResourceRenderer;
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

procedure TResourceRenderersList.Unprepare(ANode: TVRMLNode);
var
  I: integer;
begin
  { Call Unprepare and release related instance. }

  I := NodeIndex(ANode);
  if I >= 0 then
  begin
    Items[I].Unprepare;
    Items[I].Free;
    Delete(I);
  end;
end;

procedure TResourceRenderersList.UnprepareAll;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Items[I].Unprepare;
    Items[I].Free;
  end;
  Count := 0;
end;

{$endif}
