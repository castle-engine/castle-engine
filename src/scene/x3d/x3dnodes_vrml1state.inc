{%MainUnit x3dnodes.pas}
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
    Owned: array [TVRML1StateNode] of boolean;
    FNodes: array [TVRML1StateNode] of TX3DNode;
    function GetNode(const StateNode: TVRML1StateNode): TX3DNode;
    procedure SetNode(const StateNode: TVRML1StateNode; const Node: TX3DNode);
  public
    function Coordinate3: TCoordinate3Node_1;
    function ShapeHints: TShapeHintsNode_1;
    function FontStyle: TFontStyleNode_1;
    function Material: TMaterialNode_1;
    function MaterialBinding: TMaterialBindingNode_1;
    function Normal: TNormalNode;
    function NormalBinding: TNormalBindingNode_1;
    function Texture2: TTexture2Node_1;
    function TextureCoordinate2: TTextureCoordinate2Node_1;

    { Destructor automatically frees and nils all @link(Nodes) that are owned. }
    destructor Destroy; override;

    { Last nodes that occured when visiting this VRML 1.0 graph,
      indexed by TVRML1StateNode. }
    property Nodes [const StateNode: TVRML1StateNode]: TX3DNode read GetNode write SetNode;

    { Create and assign all @link(Nodes). All nodes become owned. }
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

function TVRML1State.Coordinate3       : TCoordinate3Node_1       ; begin Result := TCoordinate3Node_1       (FNodes[vsCoordinate3       ]); end;
function TVRML1State.ShapeHints        : TShapeHintsNode_1        ; begin Result := TShapeHintsNode_1        (FNodes[vsShapeHints        ]); end;
function TVRML1State.FontStyle         : TFontStyleNode_1         ; begin Result := TFontStyleNode_1         (FNodes[vsFontStyle         ]); end;
function TVRML1State.Material          : TMaterialNode_1          ; begin Result := TMaterialNode_1          (FNodes[vsMaterial          ]); end;
function TVRML1State.MaterialBinding   : TMaterialBindingNode_1   ; begin Result := TMaterialBindingNode_1   (FNodes[vsMaterialBinding   ]); end;
function TVRML1State.Normal            : TNormalNode              ; begin Result := TNormalNode              (FNodes[vsNormal            ]); end;
function TVRML1State.NormalBinding     : TNormalBindingNode_1     ; begin Result := TNormalBindingNode_1     (FNodes[vsNormalBinding     ]); end;
function TVRML1State.Texture2          : TTexture2Node_1          ; begin Result := TTexture2Node_1          (FNodes[vsTexture2          ]); end;
function TVRML1State.TextureCoordinate2: TTextureCoordinate2Node_1; begin Result := TTextureCoordinate2Node_1(FNodes[vsTextureCoordinate2]); end;

function TVRML1State.GetNode(const StateNode: TVRML1StateNode): TX3DNode;
begin
  Result := FNodes[StateNode];
end;

procedure TVRML1State.SetNode(
  const StateNode: TVRML1StateNode; const Node: TX3DNode);
begin
  if FNodes[StateNode] <> Node then
  begin
    {$warnings off} // using deprecated to keep supporting VRML 1
    Assert((Node = nil) or (Node is VRML1StateClasses[StateNode]));
    {$warnings on}

    if Owned[StateNode] then
      FNodes[StateNode].Free;
    FNodes[StateNode] := Node;
    Owned[StateNode] := false;
  end;
end;

procedure TVRML1State.CreateNodes;
var
  SN: TVRML1StateNode;
begin
  for SN := Low(SN) to High(SN) do
  begin
    {$warnings off} // using deprecated to keep supporting VRML 1
    Nodes[SN] := VRML1StateClasses[SN].Create;
    {$warnings on}
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
        FNodes[SN].Free;
      FNodes[SN] := nil;
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
    Nodes[SN] := Source.FNodes[SN]; // this is called so often that even changing Nodes to FNodes makes a difference
end;

procedure TVRML1State.SetOwnNode(const StateNode: TVRML1StateNode;
  const Node: TX3DNode);
begin
  Nodes[StateNode] := Node;
  Owned[StateNode] := true;
end;

{$endif read_implementation}
