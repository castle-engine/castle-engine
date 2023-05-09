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
  { Base X3D node that can have some metadata. }
  TAbstractNode = class(TX3DNode)
  strict private
    function GetMetadataBooleanArray(const Key: String; const Index: Cardinal): Boolean;
    procedure SetMetadataBooleanArray(const Key: String; const Index: Cardinal; const Value: Boolean);
    function GetMetadataStringArray(const Key: String; const Index: Cardinal): String;
    procedure SetMetadataStringArray(const Key: String; const Index: Cardinal; const Value: String);
    function GetMetadataIntegerArray(const Key: String; const Index: Cardinal): Integer;
    procedure SetMetadataIntegerArray(const Key: String; const Index: Cardinal; const Value: Integer);
    function GetMetadataDoubleArray(const Key: String; const Index: Cardinal): Double;
    procedure SetMetadataDoubleArray(const Key: String; const Index: Cardinal; const Value: Double);

    function GetMetadataBoolean(const Key: String): Boolean;
    procedure SetMetadataBoolean(const Key: String; const Value: Boolean);
    function GetMetadataString(const Key: String): String;
    procedure SetMetadataString(const Key: String; const Value: String);
    function GetMetadataInteger(const Key: String): Integer;
    procedure SetMetadataInteger(const Key: String; const Value: Integer);
    function GetMetadataDouble(const Key: String): Double;
    procedure SetMetadataDouble(const Key: String; const Value: Double);
  public
    procedure InternalInsertMetadata(const M: TAbstractMetadataNode);

    { Find a "metadata" information within this node with given key.
      Returns @nil if not found.

      This seeks for a TAbstractMetadataNode instance within the X3D "metadata"
      field  of this node. Looks for TAbstractMetadataNode with
      @link(TAbstractMetadataNode.NameField) matching the parameter @code(Key).
      Note that X3D spec allows multiple metadata nodes to share the same key,
      this method only finds the first one.

      It ignores TMetadataSet when searching, as TMetadataSet doesn't contain
      any simple data, it serves only as container for other metadata nodes.
    }
    function FindMetadata(const Key: String): TAbstractMetadataNode;

    { Get/set metadata, identified by a given Key, as a Boolean.
      "Metadata" is any value you want to save with this node, it has no effect on the rendering/behavior.
      This property hides various complexities of the X3D metadata nodes.

      This property looks for a TMetadataBooleanNode node inside the "metadata" of this node
      using @link(FindMetadata).
      If found, it allows to operate @link(TMetadataBooleanNode.FdValue) array.
      The MetadataBooleanArray allows to get/set appropriate items of this array.
      The MetadataBoolean simply gets/sets the first array item.

      If such metadata node is not found,
      or it's @link(TMetadataBooleanNode.FdValue) array is too short,
      this returns false.
      When setting, it adds such metadata node if necessary,
      and increases the @link(TMetadataBooleanNode.FdValue) array length if necessary. }
    property MetadataBoolean[const Key: String]: Boolean read GetMetadataBoolean write SetMetadataBoolean;
    property MetadataBooleanArray[const Key: String; const Index: Cardinal]: Boolean read GetMetadataBooleanArray write SetMetadataBooleanArray;

    { Get/set metadata, identified by a given Key, as a String.
      "Metadata" is any value you want to save with this node, it has no effect on the rendering/behavior.
      This property hides various complexities of the X3D metadata nodes.

      This property looks for a TMetadataStringNode node inside the "metadata" of this node
      using @link(FindMetadata).
      If found, it allows to operate @link(TMetadataStringNode.FdValue) array.
      The MetadataStringArray allows to get/set appropriate items of this array.
      The MetadataString simply gets/sets the first array item.

      If such metadata node is not found,
      or it's @link(TMetadataStringNode.FdValue) array is too short,
      this returns ''.
      When setting, it adds such metadata node if necessary,
      and increases the @link(TMetadataStringNode.FdValue) array length if necessary. }
    property MetadataString[const Key: String]: String read GetMetadataString write SetMetadataString;
    property MetadataStringArray[const Key: String; const Index: Cardinal]: String read GetMetadataStringArray write SetMetadataStringArray;

    { Get/set metadata, identified by a given Key, as a Integer.
      "Metadata" is any value you want to save with this node, it has no effect on the rendering/behavior.
      This property hides various complexities of the X3D metadata nodes.

      This property looks for a TMetadataIntegerNode node inside the "metadata" of this node
      using @link(FindMetadata).
      If found, it allows to operate @link(TMetadataIntegerNode.FdValue) array.
      The MetadataIntegerArray allows to get/set appropriate items of this array.
      The MetadataInteger simply gets/sets the first array item.

      If such metadata node is not found,
      or it's @link(TMetadataIntegerNode.FdValue) array is too short,
      this returns 0.
      When setting, it adds such metadata node if necessary,
      and increases the @link(TMetadataIntegerNode.FdValue) array length if necessary. }
    property MetadataInteger[const Key: String]: Integer read GetMetadataInteger write SetMetadataInteger;
    property MetadataIntegerArray[const Key: String; const Index: Cardinal]: Integer read GetMetadataIntegerArray write SetMetadataIntegerArray;

    { Get/set metadata, identified by a given Key, as a Double.
      "Metadata" is any value you want to save with this node, it has no effect on the rendering/behavior.
      This property hides various complexities of the X3D metadata nodes.

      This property looks for a TMetadataDoubleNode or TMetadataFloatNode
      node inside the "metadata" of this node  using @link(FindMetadata).
      If found, it allows to operate @link(TMetadataDoubleNode.FdValue) array
      or @link(TMetadataFloatNode.FdValue) array.
      The MetadataDoubleArray allows to get/set appropriate items of this array.
      The MetadataDouble simply gets/sets the first array item.

      If such metadata node is not found,
      or it's @link(TMetadataDoubleNode.FdValue) array is too short,
      this returns 0.0.
      When setting, it adds such metadata node if necessary,
      and increases the @link(TMetadataDoubleNode.FdValue) array length if necessary. }
    property MetadataDouble[const Key: String]: Double read GetMetadataDouble write SetMetadataDouble;
    property MetadataDoubleArray[const Key: String; const Index: Cardinal]: Double read GetMetadataDoubleArray write SetMetadataDoubleArray;

    {$I auto_generated_node_helpers/x3dnodes_x3dnode.inc}
  end;

  { Basis for all metadata nodes. }
  TAbstractMetadataNode = class(TAbstractNode)
    {$I auto_generated_node_helpers/x3dnodes_x3dmetadatanode.inc}
  end;

  { Abstract node type that indicates that the node may be used as
    a child of a grouping node,
    e.g. inside @link(TAbstractGroupingNode.FdChildren). }
  TAbstractChildNode = class(TAbstractNode)
    {$I auto_generated_node_helpers/x3dnodes_x3dchildnode.inc}
  end;

  { Abstract base type for all bindable children nodes. }
  TAbstractBindableNode = class(TAbstractChildNode)
  strict private
    FBound: Boolean;
    FTransform: TMatrix4;
    FTransformScale: Single;
    procedure EventIsBoundReceive(const Event: TX3DEvent; const AValue: TX3DField;
      const Time: TX3DTime);
    procedure SetBound(const Value: Boolean);
  protected
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;

    { "Bound" node is the node currently used, for example the "bound" background
      determines the current background look.
      Setting this to @true means that the node becomes the current node
      of the given type in the scene.

      In every scene (TCastleScene), there are four "stacks" of bindable nodes:

      @orderedList(
        @item(Viewpoints determine the camera position/orientation,
          field of view, orthographic/perspective projection etc.)
        @item(NavigationInfo determines the current navigation type,
          avatar size etc.)
        @item(Background determines the dispayed background (like a skybox
          or a simple solid color).)
        @item(Fog determines the current fog.)
      )

      The top-most node of each stack is called "bound".
      If the scene is set as TCastleRootTransform.MainScene,
      then the "bound" node determines the camera, background or fog settings.

      Setting this property to @true sends an X3D event "set_bind := true".
      This adds the node to the stack, and makes sure it is at the top.
      Setting this to @false pops the node from the stack.
      See the X3D specification about the "bindable nodes" foe details:
      http://www.web3d.org/documents/specifications/19775-1/V3.3/Part01/components/core.html#BindableChildrenNodes
    }
    property Bound: Boolean read FBound write SetBound;

    { Transformation of this bindable node.

      Bound nodes cannot be instantiated multiple number of times
      (this would make simple event like "set_bind" not possible,
      as it would not be known in what coordinate space the node is bound),
      so it's perfectly safe and comfortable to just keep their transformation
      here, a their property.

      It is gathered during traversing. Last BeforeTraverse call for this
      node sets Transform properties. By default, these represent identity
      transformation.

      Note that using TransformScale for bindable nodes like fog
      is a little simplification. Theoretically, the scale can be non-uniform,
      and around an arbitrary axis. So to apply e.g. fog, we should
      transform the 3D world back into local fog coordinate system,
      and calculate the distances there. Instead right now we do
      the opposite: transform stuff like fog VisibilityRange by
      TransformScale, and calculate distances in world coordinate
      system. This is a simplification, but in practice it's perfect
      (who uses non-uniform fog scale?) and it can be expressed for renderers
      (OpenGL) without any problems.

      @groupBegin }
    property Transform: TMatrix4 read FTransform;
    property TransformScale: Single read FTransformScale;
    { @groupEnd }

    {$I auto_generated_node_helpers/x3dnodes_x3dbindablenode.inc}
  end;

  { Base class for all nodes that contain only information without visual
    semantics. }
  TAbstractInfoNode = class(TAbstractChildNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dinfonode.inc}
  end;

  { Base abstract class for most (but not all) sensors.
    We may introduce TSensorFunctionality in the future if it will be necessary,
    to express X3DSensorNode exactly. }
  TAbstractSensorNode = class(TAbstractChildNode)
  {$I auto_generated_node_helpers/x3dnodes_x3dsensornode.inc}
  end;

  { A metadata (extra, custom information at a node) with a boolean type. }
  TMetadataBooleanNode = class(TAbstractMetadataNode)
  {$I auto_generated_node_helpers/x3dnodes_metadataboolean.inc}
  end;

  { A metadata (extra, custom information at a node) with
    a double-precision floating point number type. }
  TMetadataDoubleNode = class(TAbstractMetadataNode)
  {$I auto_generated_node_helpers/x3dnodes_metadatadouble.inc}
  end;

  { A metadata (extra, custom information at a node) with
    a single-precision floating point number type. }
  TMetadataFloatNode = class(TAbstractMetadataNode)
  {$I auto_generated_node_helpers/x3dnodes_metadatafloat.inc}
  end;

  { A metadata (extra, custom information at a node) with an integer type. }
  TMetadataIntegerNode = class(TAbstractMetadataNode)
  {$I auto_generated_node_helpers/x3dnodes_metadatainteger.inc}
  end;

  { A metadata (extra, custom information at a node) set,
    to reference of collection of other metadata nodes. }
  TMetadataSetNode = class(TAbstractMetadataNode)
  {$I auto_generated_node_helpers/x3dnodes_metadataset.inc}
  end;

  { A metadata (extra, custom information at a node) with a string type. }
  TMetadataStringNode = class(TAbstractMetadataNode)
  {$I auto_generated_node_helpers/x3dnodes_metadatastring.inc}
  end;

  { Information about the world.
    This node is strictly for documentation purposes
    and has no effect on the visual appearance or behavior of the world. }
  TWorldInfoNode = class(TAbstractInfoNode)
  {$I auto_generated_node_helpers/x3dnodes_worldinfo.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractNode -------------------------------------------------------------- }

function TAbstractNode.FindMetadata(const Key: String): TAbstractMetadataNode;

  function FindInSet(const MetadataSet: TMetadataSetNode): TAbstractMetadataNode; forward;

  { Search a Metadata node,
    which may be nil,
    may be TMetadataSetNode,
    may be other TAbstractMetadataNode. }
  function FindInSetOrSingle(const PossibleMetadata: TX3DNode): TAbstractMetadataNode;
  begin;
    Result := nil;
    if PossibleMetadata <> nil then
    begin
      if PossibleMetadata is TMetadataSetNode then
        Result := FindInSet(TMetadataSetNode(PossibleMetadata))
      else
      if (PossibleMetadata is TAbstractMetadataNode) and
         (TAbstractMetadataNode(PossibleMetadata).NameField = Key) then
        Result := TAbstractMetadataNode(PossibleMetadata);
    end;
  end;

  { Search a TMetadataSetNode instance (never nil). }
  function FindInSet(const MetadataSet: TMetadataSetNode): TAbstractMetadataNode;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to MetadataSet.FdValue.Count - 1 do
    begin
      Result := FindInSetOrSingle(MetadataSet.FdValue[I]);
      if Result <> nil then Exit;
    end;
  end;

begin
  {$ifdef CASTLE_SLIM_NODES}
  Result := nil;
  {$else}
  Result := FindInSetOrSingle(FdMetadata.Value);
  {$endif}
end;

procedure TAbstractNode.InternalInsertMetadata(const M: TAbstractMetadataNode);
var
  MSet: TMetadataSetNode;
begin
  {$ifndef CASTLE_SLIM_NODES}
  if FdMetadata.Value is TMetadataSetNode then
  begin
    MSet := TMetadataSetNode(FdMetadata.Value);
  end else
  begin
    MSet := TMetadataSetNode.Create;
    MSet.NameField := 'ContainerForAllMetadataValues'; // set NameField because it is required by X3D
    // preserve previous "metadata" value, by adding it to set
    if FdMetadata.Value <> nil then
      MSet.FdValue.Add(FdMetadata.Value);
    FdMetadata.Value := MSet;
  end;
  MSet.FdValue.Add(M);
  {$endif}
end;

function TAbstractNode.GetMetadataBooleanArray(const Key: String; const Index: Cardinal): Boolean;
var
  M: TAbstractMetadataNode;
  MB: TMetadataBooleanNode;
begin
  M := FindMetadata(Key);
  if M = nil then Exit(false);
  if not (M is TMetadataBooleanNode) then Exit(false);

  MB := TMetadataBooleanNode(M);
  if Index < MB.FdValue.Count then
    Result := MB.FdValue.Items[Index]
  else
    Result := false;
end;

procedure TAbstractNode.SetMetadataBooleanArray(const Key: String; const Index: Cardinal; const Value: Boolean);
var
  M: TAbstractMetadataNode;
  MB: TMetadataBooleanNode;
begin
  M := FindMetadata(Key);
  if M = nil then
  begin
    MB := TMetadataBooleanNode.Create;
    MB.NameField := Key;
    InternalInsertMetadata(MB);
  end else
  begin
    if not (M is TMetadataBooleanNode) then
      raise Exception.CreateFmt('Cannot add Boolean metadata with key "%s", there is already metadata of type %s', [
        Key,
        M.X3DType
      ]);
    MB := TMetadataBooleanNode(M);
  end;

  MB.FdValue.Count := Max(MB.FdValue.Count, Index + 1);
  MB.FdValue.Items[Index] := Value;
end;

function TAbstractNode.GetMetadataStringArray(const Key: String; const Index: Cardinal): String;
var
  M: TAbstractMetadataNode;
  MB: TMetadataStringNode;
begin
  M := FindMetadata(Key);
  if M = nil then Exit('');
  if not (M is TMetadataStringNode) then Exit('');

  MB := TMetadataStringNode(M);
  if Index < MB.FdValue.Count then
    Result := MB.FdValue.Items[Index]
  else
    Result := '';
end;

procedure TAbstractNode.SetMetadataStringArray(const Key: String; const Index: Cardinal; const Value: String);
var
  M: TAbstractMetadataNode;
  MB: TMetadataStringNode;
begin
  M := FindMetadata(Key);
  if M = nil then
  begin
    MB := TMetadataStringNode.Create;
    MB.NameField := Key;
    InternalInsertMetadata(MB);
  end else
  begin
    if not (M is TMetadataStringNode) then
      raise Exception.CreateFmt('Cannot add String metadata with key "%s", there is already metadata of type %s', [
        Key,
        M.X3DType
      ]);
    MB := TMetadataStringNode(M);
  end;

  MB.FdValue.Count := Max(MB.FdValue.Count, Index + 1);
  MB.FdValue.Items[Index] := Value;
end;

function TAbstractNode.GetMetadataIntegerArray(const Key: String; const Index: Cardinal): Integer;
var
  M: TAbstractMetadataNode;
  MB: TMetadataIntegerNode;
begin
  M := FindMetadata(Key);
  if M = nil then Exit(0);
  if not (M is TMetadataIntegerNode) then Exit(0);

  MB := TMetadataIntegerNode(M);
  if Index < MB.FdValue.Count then
    Result := MB.FdValue.Items[Index]
  else
    Result := 0;
end;

procedure TAbstractNode.SetMetadataIntegerArray(const Key: String; const Index: Cardinal; const Value: Integer);
var
  M: TAbstractMetadataNode;
  MB: TMetadataIntegerNode;
begin
  M := FindMetadata(Key);
  if M = nil then
  begin
    MB := TMetadataIntegerNode.Create;
    MB.NameField := Key;
    InternalInsertMetadata(MB);
  end else
  begin
    if not (M is TMetadataIntegerNode) then
      raise Exception.CreateFmt('Cannot add Integer metadata with key "%s", there is already metadata of type %s', [
        Key,
        M.X3DType
      ]);
    MB := TMetadataIntegerNode(M);
  end;

  MB.FdValue.Count := Max(MB.FdValue.Count, Index + 1);
  MB.FdValue.Items[Index] := Value;
end;

function TAbstractNode.GetMetadataDoubleArray(const Key: String; const Index: Cardinal): Double;
var
  M: TAbstractMetadataNode;
  MF: TMetadataFloatNode;
  MD: TMetadataDoubleNode;
begin
  Result := 0.0;

  M := FindMetadata(Key);
  if M = nil then Exit;

  if M is TMetadataDoubleNode then
  begin
    MD := TMetadataDoubleNode(M);
    if Index < MD.FdValue.Count then
      Result := MD.FdValue.Items[Index];
  end else
  if M is TMetadataFloatNode then
  begin
    MF := TMetadataFloatNode(M);
    if Index < MF.FdValue.Count then
      Result := MF.FdValue.Items[Index]
  end;
end;

procedure TAbstractNode.SetMetadataDoubleArray(const Key: String; const Index: Cardinal; const Value: Double);
var
  M: TAbstractMetadataNode;
  MF: TMetadataFloatNode;
  MD: TMetadataDoubleNode;
begin
  M := FindMetadata(Key);
  if M = nil then
  begin
    M := TMetadataDoubleNode.Create;
    M.NameField := Key;
    InternalInsertMetadata(M);
  end;

  if M is TMetadataFloatNode then
  begin
    MF := TMetadataFloatNode(M);
    MF.FdValue.Count := Max(MF.FdValue.Count, Index + 1);
    MF.FdValue.Items[Index] := Value;
    WritelnWarning('Storing double-precision metadata in single-precision MetadataFloat, some precision may be lost');
  end else
  if M is TMetadataDoubleNode then
  begin
    MD := TMetadataDoubleNode(M);
    MD.FdValue.Count := Max(MD.FdValue.Count, Index + 1);
    MD.FdValue.Items[Index] := Value;
  end else
    raise Exception.CreateFmt('Cannot add Double metadata with key "%s", there is already metadata of type %s', [
      Key,
      M.X3DType
    ]);
end;

function TAbstractNode.GetMetadataBoolean(const Key: String): Boolean;
begin
  Result := MetadataBooleanArray[Key, 0];
end;

procedure TAbstractNode.SetMetadataBoolean(const Key: String; const Value: Boolean);
begin
  MetadataBooleanArray[Key, 0] := Value;
end;

function TAbstractNode.GetMetadataString(const Key: String): String;
begin
  Result := MetadataStringArray[Key, 0];
end;

procedure TAbstractNode.SetMetadataString(const Key: String; const Value: String);
begin
  MetadataStringArray[Key, 0] := Value;
end;

function TAbstractNode.GetMetadataInteger(const Key: String): Integer;
begin
  Result := MetadataIntegerArray[Key, 0];
end;

procedure TAbstractNode.SetMetadataInteger(const Key: String; const Value: Integer);
begin
  MetadataIntegerArray[Key, 0] := Value;
end;

function TAbstractNode.GetMetadataDouble(const Key: String): Double;
begin
  Result := MetadataDoubleArray[Key, 0];
end;

procedure TAbstractNode.SetMetadataDouble(const Key: String; const Value: Double);
begin
  MetadataDoubleArray[Key, 0] := Value;
end;

{ TAbstractBindableNode ------------------------------------------------------ }

constructor TAbstractBindableNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  EventIsBound.AddNotification({$ifdef FPC}@{$endif} EventIsBoundReceive);
  FTransform := TMatrix4.Identity;
  FTransformScale := 1;
end;

procedure TAbstractBindableNode.BeforeTraverse(
  StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;

  FTransform := StateStack.Top.Transformation.Transform;
  FTransformScale := StateStack.Top.Transformation.Scale;
end;

procedure TAbstractBindableNode.EventIsBoundReceive(
  const Event: TX3DEvent; const AValue: TX3DField; const Time: TX3DTime);
begin
  FBound := (AValue as TSFBool).Value;
end;

procedure TAbstractBindableNode.SetBound(const Value: Boolean);
begin
  EventSet_bind.Send(Value);
end;

{ registration --------------------------------------------------------------- }

procedure RegisterCoreNodes;
begin
  NodesManager.RegisterNodeClasses([
    TMetadataBooleanNode,
    TMetadataDoubleNode,
    TMetadataFloatNode,
    TMetadataIntegerNode,
    TMetadataSetNode,
    TMetadataStringNode,
    TWorldInfoNode
  ]);
end;

{$endif read_implementation}
