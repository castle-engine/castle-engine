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

  { Node types used with @link(TVRML1State). }
  TVRML1StateNode =
  (
    vsCoordinate3,
    vsShapeHints,
    vsFontStyle,
    vsMaterial,
    vsMaterialBinding,
    vsNormal,
    vsNormalBinding,
    vsTexture2,
    vsTextureCoordinate2
  );

  { Nodes that are saved during VRML/X3D traversing.
    Used for @link(TX3DGraphTraverseState.VRML1State).
    These nodes affect some following nodes in the graph, in VRML 1.0.

    They are never @nil after a @link(TX3DNode.Traverse) operation.
    the traversing code always takes care to initialize
    them to default nodes at the beginning.

    For all nodes enumerated here: it's guaranteed
    they don't affect the state (@link(TX3DGraphTraverseState)) during traversing
    (that is, they don't do anything special in
    TX3DNode.BeforeTraverse / TX3DNode.MiddleTraverse / TX3DNode.AfterTraverse).
    So it's guaranteed that changing some field's value of a node
    within TVRML1State affects @italic(only)
    the shapes that have given node inside State.VRML1State.
    TCastleSceneCore.InternalChangedField depends on that. }
  TVRML1State = class
  strict private
    function GetNode(const StateNode: TVRML1StateNode): TX3DNode;
    procedure SetNode(const StateNode: TVRML1StateNode; const Node: TX3DNode);
    procedure SetNodeCore(const StateNode: TVRML1StateNode; const Node: TX3DNode);
  public
    Owned: array [TVRML1StateNode] of boolean;

    Coordinate3: TCoordinate3Node_1;
    ShapeHints: TShapeHintsNode_1;
    FontStyle: TFontStyleNode_1;
    Material: TMaterialNode_1;
    MaterialBinding: TMaterialBindingNode_1;
    Normal: TNormalNode;
    NormalBinding: TNormalBindingNode_1;
    Texture2: TTexture2Node_1;
    TextureCoordinate2: TTextureCoordinate2Node_1;
    { Additions here must be synchronized with additions to TVRML1StateNode }

    { Destructor automatically frees and nils all @link(Nodes) that are @link(Owned). }
    destructor Destroy; override;

    { Last nodes that occured when visiting this VRML 1.0 graph,
      indexed by TVRML1StateNode. }
    property Nodes [StateNode: TVRML1StateNode]: TX3DNode read GetNode write SetNode;

    { Create and assign all @link(Nodes). All nodes become @link(Owned). }
    procedure CreateNodes;

    { Assign from Source.

      During doing this, old nodes are freed if they were owned.
      New nodes are not owned.

      This takes care of checking for each TVRML1StateNode
      if the old node is equal to new one. If yes, then the node
      if not freed (regardless of "owned" status), and the "owned"
      status is left as-is (not chaned to false).
      This way calling something like @code(MyState.Assign(MyState))
      is a valid and harmless operation. }
    procedure Assign(const Source: TVRML1State);

    { Set the node, and make it owned by this class.
      Contrast this with @code(Nodes[StateNode] := Node) that makes
      the node @italic(not) owned. This is just a shortcut for
      @longCode(#
        Nodes[StateNode] := Node;
        Owned[StateNode] := true;
      #)
    }
    procedure SetOwnNode(const StateNode: TVRML1StateNode; const Node: TX3DNode);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TVRML1State ---------------------------------------------------- }

function TVRML1State.GetNode(const StateNode: TVRML1StateNode): TX3DNode;
begin
  case StateNode of
    vsCoordinate3       : Result := Coordinate3       ;
    vsShapeHints        : Result := ShapeHints        ;
    vsFontStyle         : Result := FontStyle         ;
    vsMaterial          : Result := Material          ;
    vsMaterialBinding   : Result := MaterialBinding   ;
    vsNormal            : Result := Normal            ;
    vsNormalBinding     : Result := NormalBinding     ;
    vsTexture2          : Result := Texture2          ;
    vsTextureCoordinate2: Result := TextureCoordinate2;
    else raise EInternalError.Create('TVRML1State.GetNode: StateNode?');
  end;
end;

procedure TVRML1State.SetNodeCore(
  const StateNode: TVRML1StateNode; const Node: TX3DNode);
begin
  case StateNode of
    vsCoordinate3       : TX3DNode(Coordinate3       ) := Node;
    vsShapeHints        : TX3DNode(ShapeHints        ) := Node;
    vsFontStyle         : TX3DNode(FontStyle         ) := Node;
    vsMaterial          : TX3DNode(Material          ) := Node;
    vsMaterialBinding   : TX3DNode(MaterialBinding   ) := Node;
    vsNormal            : TX3DNode(Normal            ) := Node;
    vsNormalBinding     : TX3DNode(NormalBinding     ) := Node;
    vsTexture2          : TX3DNode(Texture2          ) := Node;
    vsTextureCoordinate2: TX3DNode(TextureCoordinate2) := Node;
    else raise EInternalError.Create('TVRML1State.SetNode: StateNode?');
  end;
end;

procedure TVRML1State.SetNode(
  const StateNode: TVRML1StateNode; const Node: TX3DNode);
begin
  if Nodes[StateNode] <> Node then
  begin
    Assert((Node = nil) or (Node is VRML1StateClasses[StateNode]));

    if Owned[StateNode] then
      Nodes[StateNode].Free;
    SetNodeCore(StateNode, Node);
    Owned[StateNode] := false;
  end;
end;

procedure TVRML1State.CreateNodes;
var
  SN: TVRML1StateNode;
begin
  for SN := Low(SN) to High(SN) do
  begin
    Nodes[SN] := VRML1StateClasses[SN].Create;
    Owned[SN] := true;
  end;
end;

destructor TVRML1State.Destroy;

  procedure FreeAndNilNodes;
  var
    SN: TVRML1StateNode;
  begin
    for SN := Low(SN) to High(SN) do
    begin
      if Owned[SN] then
        Nodes[SN].Free;
      SetNodeCore(SN, nil);
    end;
  end;

begin
  FreeAndNilNodes;
  inherited;
end;

procedure TVRML1State.Assign(const Source: TVRML1State);
var
  SN: TVRML1StateNode;
begin
  for SN := Low(SN) to High(SN) do
    Nodes[SN] := Source.Nodes[SN];
end;

procedure TVRML1State.SetOwnNode(const StateNode: TVRML1StateNode;
  const Node: TX3DNode);
begin
  Nodes[StateNode] := Node;
  Owned[StateNode] := true;
end;

{$endif read_implementation}
