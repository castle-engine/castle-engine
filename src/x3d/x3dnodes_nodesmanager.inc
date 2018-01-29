{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

{ TNodesManager ------------------------------------------------------------ }

type
  { }
  ENodesManagerError = class(EX3DError);
  ENodeClassRegisterError = class(ENodesManagerError);
  TNodesManager = class
  strict private
    { Strings[] is ClassX3DType. Objects[] is the actual class
      (typecast to TX3DNodeClass is safe). }
    FRegistered: TStringList;
    function GetRegistered(Index: Integer): TX3DNodeClass;
  public
    constructor Create;
    destructor Destroy; override;

    { Make the given node class known to the parser and other routines.
      We associate the node class with it's TX3DNode.ClassX3DType
      (make sure it's not empty).

      It is OK to register two different node classes with the same node.
      For example, VRML 1.0 TConeNode_1 class and VRML 2.0/X3D TConeNode_2
      class both have a name 'Cone' (and will be correctly chosen during parsing).
      But you cannot register two times the same NodeClass.

      @groupBegin }
    procedure RegisterNodeClass(NodeClass: TX3DNodeClass);
    procedure RegisterNodeClasses(const NodeClasses: array of TX3DNodeClass);
    { @groupEnd }

    { Unregisters given node class, removing it from our table.

      @raises(ENodesManagerError if NodeClass.ClassX3DType = ''
        (so it cannot be even registered), or if
        ((NodeClass was not registered) and ErrorIfNotRegistered)) }
    procedure UnRegisterNodeClass(NodeClass: TX3DNodeClass;
      ErrorIfNotRegistered: boolean = true);

    { Return node class for a given name. This method is the main purpose
      of TNodesManager: to map node names into node classes.

      Searches in nodes registered by RegisterNodeClass and such.
      During searching, looks not only for matching node name,
      but also at matching VRML/X3D version, checking
      @code(ForVRMLVersion(Version)).

      Returns @nil when not found. }
    function X3DTypeToClass(const AX3DType: string;
      const Version: TX3DVersion): TX3DNodeClass;

    { Return class that matches given URL. This is useful for EXTERNROTOs.
      Returns @nil if not found. }
    function URNToClass(const URN: string): TX3DNodeClass;

    { Enumerate all registered classes, from Registered[0] to
      Registered[RegisteredCount - 1].

      @groupBegin }
    property Registered [Index: Integer]: TX3DNodeClass read GetRegistered;
    function RegisteredCount: Cardinal;
    { @groupEnd }
  end;

var
  { Nodes manager instance. In normal circumstances, this is the only
    instance of TNodesManager class ever created. It is created / destroyed
    in this unit's initialization / finalization. }
  NodesManager: TNodesManager;

{$endif read_interface}

{$ifdef read_implementation}

{ TNodesManager ------------------------------------------------------------ }

constructor TNodesManager.Create;
begin
  inherited;
  FRegistered := TStringListCaseSens.Create;
end;

destructor TNodesManager.Destroy;
begin
  FRegistered.Free;
  inherited;
end;

procedure TNodesManager.RegisterNodeClass(NodeClass: TX3DNodeClass);
begin
  if NodeClass.ClassX3DType = '' then
    raise ENodesManagerError.Create('Class '+NodeClass.ClassName+' has '+
      'empty ClassX3DType so it cannot be registered in TNodesManager');

  if FRegistered.IndexOfObject(TObject(Pointer(NodeClass))) <> -1 then
    raise ENodesManagerError.Create('Class '+NodeClass.ClassName+
      ' was already registered in TNodesManager');

  FRegistered.AddObject(NodeClass.ClassX3DType, TObject(Pointer(NodeClass)));
end;

procedure TNodesManager.RegisterNodeClasses(
  const NodeClasses: array of TX3DNodeClass);
var
  I: Integer;
begin
  for i := 0 to High(NodeClasses) do RegisterNodeClass(NodeClasses[i]);
end;

procedure TNodesManager.UnRegisterNodeClass(NodeClass: TX3DNodeClass;
  ErrorIfNotRegistered: boolean);
var i: Integer;
begin
  if NodeClass.ClassX3DType = '' then
    raise ENodesManagerError.Create('Class '+NodeClass.ClassName+' has '+
      'empty ClassX3DType so it cannot be unregistered (or even registered) '+
      'in TNodesManager');

  i := FRegistered.IndexOfObject(TObject(Pointer(NodeClass)));
  if i <> - 1 then
    FRegistered.Delete(i) else
  if ErrorIfNotRegistered then
    ENodesManagerError.Create('Node class "' + NodeClass.ClassName +
      '" was not registered, so you cannot unregister it');
end;

function TNodesManager.X3DTypeToClass(const AX3DType: string;
  const Version: TX3DVersion): TX3DNodeClass;
var
  I: Integer;
begin
  for I := 0 to FRegistered.Count - 1 do
  begin
    Result := TX3DNodeClass(FRegistered.Objects[I]);
    if (FRegistered[I] = AX3DType) and
       Result.ForVRMLVersion(Version) then
      Exit;
  end;
  Result := nil;
end;

function TNodesManager.URNToClass(const URN: string): TX3DNodeClass;
var
  I: Integer;
begin
  for I := 0 to FRegistered.Count - 1 do
  begin
    Result := TX3DNodeClass(FRegistered.Objects[I]);
    if Result.URNMatching(URN) then
      Exit;
  end;
  Result := nil;
end;

function TNodesManager.GetRegistered(Index: Integer): TX3DNodeClass;
begin
  Result := TX3DNodeClass(FRegistered.Objects[Index]);
end;

function TNodesManager.RegisteredCount: Cardinal;
begin
  Result := FRegistered.Count;
end;

{$endif read_implementation}
