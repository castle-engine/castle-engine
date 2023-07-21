{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { }
  TX3DNodeNameRec = record
    Node: TX3DNode;
    Name: string;
    Finished: boolean;
    function DeepCopy(const CopyState: TX3DNodeDeepCopyState): TX3DNodeNameRec;
  end;
  PX3DNodeNameRec = ^TX3DNodeNameRec;

  { List to track node names while parsing VRML/X3D file. }
  TX3DNodeNames = class({$ifdef FPC}specialize{$endif} TStructList<TX3DNodeNameRec>)
  strict private
    FAutoRemove: boolean;
    procedure DestructionNotification(const Node: TX3DNode);
    function IndexOfNode(const Node: TX3DNode): Integer;
    function IndexOfName(const Name: string): Integer;
  public
    constructor Create(const AAutoRemove: boolean);
    destructor Destroy; override;

    { If @true (determined at construction time),
      then destroyed nodes will be automatically removed from this list.
      This allows you to safely destroy node instances during this
      objects lifetime, without worrying that some dangling pointers remain
      on this list.

      Internally, this is done by registering
      itself for AnyNodeDestructionNotifications. }
    property AutoRemove: boolean read FAutoRemove;

    { Associate given node with it's own name.

      If not NodeFinished, then we understand that we're parsing / saving
      the node's contents now. If NodeFinished, then we know the node
      contents are fully parsed / saved now. This information helps
      us to detect cycles in X3D DEF/USE graph.
      For now, we just disallow such cycles. Still, we allow
      ROUTEs from inside the node, so calling with NodeFinished = false
      is still useful for parsing. }
    procedure Bind(const Node: TX3DNode; const NodeFinished: boolean); overload;
    procedure Bind(const Node: TX3DNode; const NodeFinished: boolean; const BindToName: string); overload;

    { Find node bound to given name. @nil if none. }
    function Bound(const Name: string; out NodeFinished: boolean): TX3DNode; overload;

    { Node is during reading, judging from Bind calls with NodeFinished=@false
      (when node reading starts) and with NodeFinished=@true (when node reading
      finishes). }
    function NodeDuringReading(const Node: TX3DNode): Boolean;

    { Check is Node bound in the current namespace.
      @false means that node is not within this namespace,
      possibly it's name was hidden by other node with the same name.

      Doesn't check is Node bound to it's name (Node.Name) or something
      else. So this assumes that node can only be bound (if at all)
      only to it's own name, which is true during parsing
      (when nothing can change in the middle of parsing). }
    function Bound(Node: TX3DNode): boolean; overload;

    function DeepCopy(const CopyState: TX3DNodeDeepCopyState): TX3DNodeNames;
  end;

  TX3DPrototypeNames = class(TStringListCaseSens)
  public
    procedure Bind(Proto: TX3DPrototypeBase);

    { Find proto bound to given name. @nil if none. }
    function Bound(const Name: string): TX3DPrototypeBase;
  end;

  TX3DImportableNames = class(TStringListCaseSens)
  public
    destructor Destroy; override;

    { Bind Exported names to given Inline node name.
      Exported instance becomes owner by this TX3DImportableNames instance.
      InlineName must be <> '' here. }
    procedure Bind(const InlineName: string; Exported: TX3DNodeNames);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DNodeNameRec ------------------------------------------------------------ }

function TX3DNodeNameRec.DeepCopy(const CopyState: TX3DNodeDeepCopyState): TX3DNodeNameRec;
begin
  Result.Node := CopyState.DeepCopy(Node);
  Result.Name := Name;
  Result.Finished := Finished;
end;

{ TX3DNodeNames ----------------------------------------------------------- }

constructor TX3DNodeNames.Create(const AAutoRemove: boolean);
begin
  inherited Create;
  FAutoRemove := AAutoRemove;
  if AutoRemove then
    AnyNodeDestructionNotifications.Add({$ifdef FPC}@{$endif} DestructionNotification);
end;

destructor TX3DNodeNames.Destroy;
begin
  { This may happen after X3DNodes unit finalization
    (e.g. simplest_vrml_browser_with_shadow_volumes demo_models/shadow_volumes/stonehenge.wrl,
    where TX3DRootNode with some ExportedNames is freed from CastleWindow
    unit finalization, because Application owns Window that owns Scene).
    So secure from AnyNodeDestructionNotifications being nil. }
  if AutoRemove and (AnyNodeDestructionNotifications <> nil) then
    AnyNodeDestructionNotifications.Remove({$ifdef FPC}@{$endif} DestructionNotification);
  inherited;
end;

function TX3DNodeNames.IndexOfNode(const Node: TX3DNode): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].Node = Node then
      Exit;
  Result := -1;
end;

function TX3DNodeNames.IndexOfName(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if L[Result].Name = Name then
      Exit;
  Result := -1;
end;

function TX3DNodeNames.NodeDuringReading(const Node: TX3DNode): Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if L[I].Node = Node then
      Exit(not L[I].Finished);
  Result := false; // Node reading not even started
end;

procedure TX3DNodeNames.DestructionNotification(const Node: TX3DNode);
var
  I: Integer;
begin
  I := IndexOfNode(Node);
  if I >= 0 then
    Delete(I);
end;

procedure TX3DNodeNames.Bind(const Node: TX3DNode; const NodeFinished: boolean; const BindToName: string);
var
  I: Integer;
  P: PX3DNodeNameRec;
begin
  if BindToName <> '' then
  begin
    I := IndexOfName(BindToName);
    if I <> -1 then
    begin
      P := PX3DNodeNameRec(Ptr(I));
      { Warn about duplicate node names.
        Avoid warning for TX3DPrototypeNode, as right now we knowingly
        override names when instantiating node.
        Testcase: demo-models/x3d/anchor_test.x3dv }
      if (P^.Node <> Node) and
         (not (P^.Node is TX3DPrototypeNode)) and
         (not (Node is TX3DPrototypeNode)) then
      begin
        WritelnWarning('Node name "%s" used multiple times for different nodes. This is not allowed in X3D. (It is allowed in VRML, but we advise to upgrade all VRML models to X3D.)', [
          BindToName
        ]);
      end;
    end else
      P := PX3DNodeNameRec(Add);
    P^.Node := Node;
    P^.Name := BindToName;
    P^.Finished := NodeFinished;
  end;
end;

procedure TX3DNodeNames.Bind(const Node: TX3DNode; const NodeFinished: boolean);
begin
  Bind(Node, NodeFinished, Node.X3DName);
end;

function TX3DNodeNames.Bound(const Name: string; out NodeFinished: boolean): TX3DNode;
var
  I: Integer;
begin
  I := IndexOfName(Name);
  if I <> -1 then
  begin
    Result := L[I].Node;
    NodeFinished := L[I].Finished;
  end else
    Result := nil;
end;

function TX3DNodeNames.Bound(Node: TX3DNode): boolean;
begin
  Result := IndexOfNode(Node) <> -1;
end;

function TX3DNodeNames.DeepCopy(const CopyState: TX3DNodeDeepCopyState): TX3DNodeNames;
var
  I: Integer;
begin
  Result := TX3DNodeNames.Create(AutoRemove);
  Result.Count := Count;
  for I := 0 to Count - 1 do
    Result.Items[I] := Items[I].DeepCopy(CopyState);
end;

{ TX3DPrototypeNames -------------------------------------------------------- }

procedure TX3DPrototypeNames.Bind(Proto: TX3DPrototypeBase);
var
  I: Integer;
begin
  I := IndexOf(Proto.X3DName);
  if I <> - 1 then
    Objects[I] := Proto else
    AddObject(Proto.X3DName, Proto);
end;

function TX3DPrototypeNames.Bound(const Name: string): TX3DPrototypeBase;
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I <> -1 then
    Result := Objects[I] as TX3DPrototypeBase else
    Result := nil;
end;

{ TX3DImportableNames ------------------------------------------------------- }

destructor TX3DImportableNames.Destroy;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Objects[I].Free;
  inherited;
end;

procedure TX3DImportableNames.Bind(const InlineName: string; Exported: TX3DNodeNames);
var
  I: Integer;
begin
  I := IndexOf(InlineName);
  if I = -1 then
    AddObject(InlineName, Exported) else
  begin
    Objects[I].Free;
    Objects[I] := Exported;
  end;
end;

{$endif read_implementation}
