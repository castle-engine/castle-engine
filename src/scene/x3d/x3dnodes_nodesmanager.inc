{%MainUnit x3dnodes.pas}
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

  { Manages X3D non-abstract node classes, that can be used in X3D files.
    Use only through a singleton @link(NodesManager). }
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
      Checks also @code(ForVRMLVersion(Version)).
      Returns @nil if not found. }
    function URNToClass(const URN: string;
      const Version: TX3DVersion): TX3DNodeClass;

    { Enumerate all registered classes, from Registered[0] to
      Registered[RegisteredCount - 1].

      @groupBegin }
    property Registered [Index: Integer]: TX3DNodeClass read GetRegistered;
    function RegisteredCount: Cardinal;
    { @groupEnd }
  end;

var
  { Singleton to manage X3D non-abstract node classes, that can be used in X3D files.

    In normal circumstances, this is the only
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

function TNodesManager.URNToClass(const URN: string;
  const Version: TX3DVersion): TX3DNodeClass;

  { Does this node name match the given URN. This is used when searching for an built-in
    implementation of a node declared in X3D using the EXTERNPROTO.

    In the past, this was using a virtual URNMatching implementation at each node,
    which tried to be smart:
    - use CGE prefix only for CGE extensions
    - use X3D only for X3D nodes (version >= 3)
    - use VRML 97 only for VRML 97 nodes (version 2)
    etc.

    Ultimately, this was causing a lot of maintenance burden,
    and implementation of URNMatching was often untested and slightly incorrect,
    and was useless.

    - Standard URNs for nodes are not widely used,
    - and most modern formats are just X3D anyway,
    - and we can check ForVRMLVersion.
    So we can implement them simpler. }
  function URNMatching(const URN, ClassX3DType: String): Boolean;
  begin
    Result :=
      (URN = 'urn:castle-engine.io:node:' + ClassX3DType) or
      (URN = 'urn:castle-engine.sourceforge.io:node:' + ClassX3DType) or
      (URN = 'urn:castle-engine.sourceforge.net:node:' + ClassX3DType) or
      (URN = 'urn:vrmlengine.sourceforge.net:node:' + ClassX3DType) or

      { URNs used to indicate standard VRML / X3D nodes.

        Funny thing, I actually didn't found anywhere a definite official
        statement that they are using such-and-such URNs.

        X3D specification refers to RFC
        [http://www.ietf.org/rfc/rfc3541.txt?number=3541] which, basically,
        just says "we like URNs and we'll use them" and nothing more.
        Same thing for VRML 97 spec
        [http://www.web3d.org/x3d/specifications/vrml/ISO-IEC-14772-VRML97/part1/extensions.html].
        There is no precise answer e.g. what URN should be used to
        Indicate some standard VRML 97 / X3D node.

        I constructed URNs below looking at examples in the RFC,
        annotated by a funny line "The following examples are not
        guaranteed to be real. They are presented for pedagogical reasons only." }
      (URN = 'urn:web3d:vrml97:node:' + ClassX3DType) or
      (URN = 'urn:web3d:x3d:node:' + ClassX3DType) or

      { URN to indicate BitManagement nodes. This should work, according to
        http://www.bitmanagement.com/developer/contact/examples/layer/index.html
        example EXTERNPROTO. }
      (URN = 'urn:inet:bitmanagement.de:node:' + ClassX3DType);
  end;

var
  I: Integer;
begin
  for I := 0 to FRegistered.Count - 1 do
  begin
    Result := TX3DNodeClass(FRegistered.Objects[I]);
    if URNMatching(URN, Result.ClassX3DType) and
       Result.ForVRMLVersion(Version) then
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
