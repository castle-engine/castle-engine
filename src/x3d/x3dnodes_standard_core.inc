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

  { Interface for a base X3D node that can have some metadata. }
  IAbstractNode = interface(IX3DNode)
  ['{399AC6B2-C2B3-4EEA-AB74-D52C4836B3B9}']
    property FdMetadata: TSFNode;
  end;

  { Basis for all metadata nodes. }
  IAbstractMetadataNode = interface(IX3DNode)
  ['{2D631FBF-683B-4259-A98D-74589893447C}']
    property FdName: TSFString { read GetFdname }; { }
    property FdReference: TSFString { read GetFdreference }; { }
  end;

  { Base X3D node that can have some metadata.

    Almost all X3D nodes inherit from this, with the only exception being
    the X3DMetadataObject (expressed only as an interface in Pascal,
    @link(IAbstractMetadataNode)).
    This means that you cannot have metadata inside a metadata. }
  TAbstractNode = class(TX3DNode, IAbstractNode)
  public
    procedure CreateNode; override;

    strict private FFdMetadata: TSFNode;
    public property FdMetadata: TSFNode read FFdMetadata;

    {$I auto_generated_node_helpers/x3dnodes_x3dnode.inc}
  end;

  { Interface for an abstract node type that indicates that the node may be used as
    a child of a grouping node,
    e.g. inside @link(TAbstractX3DGroupingNode.FdChildren). }
  IAbstractChildNode = interface(IAbstractNode)
  ['{ECE6F71D-CA90-4B6C-8835-EAA68EEDAEED}']
  end;

  { Abstract node type that indicates that the node may be used as
    a child of a grouping node,
    e.g. inside @link(TAbstractX3DGroupingNode.FdChildren). }
  TAbstractChildNode = class(TAbstractNode, IAbstractChildNode)
  public
    procedure CreateNode; override;

    {$I auto_generated_node_helpers/x3dnodes_x3dchildnode.inc}
  end;

  { Abstract base type for all bindable children nodes. }
  TAbstractBindableNode = class(TAbstractChildNode)
  strict private
    FBound: Boolean;
    FTransform: TMatrix4;
    FTransformScale: Single;
    procedure EventIsBoundReceive(Event: TX3DEvent; AValue: TX3DField;
      const Time: TX3DTime);
    procedure SetBound(const Value: Boolean);
  protected
    procedure BeforeTraverse(StateStack: TX3DGraphTraverseStateStack); override;
  public
    procedure CreateNode; override;

    { Event in } { }
    strict private FEventSet_bind: TSFBoolEvent;
    public property EventSet_bind: TSFBoolEvent read FEventSet_bind;

    { Event out } { }
    strict private FEventBindTime: TSFTimeEvent;
    public property EventBindTime: TSFTimeEvent read FEventBindTime;

    { Event out } { }
    strict private FEventIsBound: TSFBoolEvent;
    public property EventIsBound: TSFBoolEvent read FEventIsBound;

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
      If the scene is set as TCastleSceneManager.MainScene,
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

  { Base interface type for all sensors.
    All sensors implement this
    (and most, but not all, sensors descend also from @link(TAbstractSensorNode).) }
  IAbstractSensorNode = interface(IAbstractChildNode)
  ['{7434C62F-8084-40C3-AA57-08F9B574655A}']
    property FdEnabled: TSFBool;

    { Event: out } { }
    property EventIsActive: TSFBoolEvent;
  end;

  { Base abstract class for all sensors. }
  TAbstractSensorNode = class(TAbstractChildNode, IAbstractSensorNode)
  public
    procedure CreateNode; override;

    strict private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    { Event out } { }
    strict private FEventIsActive: TSFBoolEvent;
    public property EventIsActive: TSFBoolEvent read FEventIsActive;

    {$I auto_generated_node_helpers/x3dnodes_x3dsensornode.inc}
  end;

  { A metadata (extra, custom information at a node) with a boolean type. }
  TMetadataBooleanNode = class(TAbstractNode, IAbstractMetadataNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    strict private FFdReference: TSFString;
    public property FdReference: TSFString read FFdReference;

    strict private FFdValue: TMFBool;
    public property FdValue: TMFBool read FFdValue;

    {$I auto_generated_node_helpers/x3dnodes_metadataboolean.inc}
  end;

  { A metadata (extra, custom information at a node) with
    a double-precision floating point number type. }
  TMetadataDoubleNode = class(TAbstractNode, IAbstractMetadataNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    strict private FFdReference: TSFString;
    public property FdReference: TSFString read FFdReference;

    strict private FFdValue: TMFDouble;
    public property FdValue: TMFDouble read FFdValue;

    {$I auto_generated_node_helpers/x3dnodes_metadatadouble.inc}
  end;

  { A metadata (extra, custom information at a node) with
    a single-precision floating point number type. }
  TMetadataFloatNode = class(TAbstractNode, IAbstractMetadataNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    strict private FFdReference: TSFString;
    public property FdReference: TSFString read FFdReference;

    strict private FFdValue: TMFFloat;
    public property FdValue: TMFFloat read FFdValue;

    {$I auto_generated_node_helpers/x3dnodes_metadatafloat.inc}
  end;

  { A metadata (extra, custom information at a node) with an integer type. }
  TMetadataIntegerNode = class(TAbstractNode, IAbstractMetadataNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    strict private FFdReference: TSFString;
    public property FdReference: TSFString read FFdReference;

    strict private FFdValue: TMFInt32;
    public property FdValue: TMFInt32 read FFdValue;

    {$I auto_generated_node_helpers/x3dnodes_metadatainteger.inc}
  end;

  { A metadata (extra, custom information at a node) set,
    to reference of collection of other metadata nodes. }
  TMetadataSetNode = class(TAbstractNode, IAbstractMetadataNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    strict private FFdReference: TSFString;
    public property FdReference: TSFString read FFdReference;

    strict private FFdValue: TMFNode;
    public property FdValue: TMFNode read FFdValue;

    {$I auto_generated_node_helpers/x3dnodes_metadataset.inc}
  end;

  { A metadata (extra, custom information at a node) with a string type. }
  TMetadataStringNode = class(TAbstractNode, IAbstractMetadataNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdName: TSFString;
    public property FdName: TSFString read FFdName;

    strict private FFdReference: TSFString;
    public property FdReference: TSFString read FFdReference;

    strict private FFdValue: TMFString;
    public property FdValue: TMFString read FFdValue;

    {$I auto_generated_node_helpers/x3dnodes_metadatastring.inc}
  end;

  { Information about the world.
    This node is strictly for documentation purposes
    and has no effect on the visual appearance or behaviour of the world. }
  TWorldInfoNode = class(TAbstractInfoNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    strict private FFdInfo: TMFString;
    public property FdInfo: TMFString read FFdInfo;

    strict private FFdTitle: TSFString;
    public property FdTitle: TSFString read FFdTitle;

    {$I auto_generated_node_helpers/x3dnodes_worldinfo.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure TAbstractNode.CreateNode;
begin
  inherited;

  FFdMetadata := TSFNode.Create(Self, true, 'metadata', IAbstractMetadataNode);
  AddField(FFdMetadata);
end;

procedure TAbstractChildNode.CreateNode;
begin
  inherited;

  { It's natural that X3DChildNode goes into "children" node of parent.
    Actually, this makes many other
      DefaultContainerField := 'children'
    lines in our x3d_*.inc include files useless. }
  DefaultContainerField := 'children';
end;

procedure TAbstractBindableNode.CreateNode;
begin
  inherited;

  FEventSet_bind := TSFBoolEvent.Create(Self, 'set_bind', true);
  AddEvent(FEventSet_bind);

  FEventBindTime := TSFTimeEvent.Create(Self, 'bindTime', false);
  AddEvent(FEventBindTime);

  FEventIsBound := TSFBoolEvent.Create(Self, 'isBound', false);
   EventIsBound.AddNotification({$ifdef CASTLE_OBJFPC}@{$endif} EventIsBoundReceive);
  AddEvent(FEventIsBound);

  FTransform := TMatrix4.Identity;
  FTransformScale := 1;
end;

procedure TAbstractBindableNode.BeforeTraverse(
  StateStack: TX3DGraphTraverseStateStack);
begin
  inherited;

  FTransform := StateStack.Top.Transform;
  FTransformScale := StateStack.Top.TransformScale;
end;

procedure TAbstractBindableNode.EventIsBoundReceive(
  Event: TX3DEvent; AValue: TX3DField; const Time: TX3DTime);
begin
  FBound := (AValue as TSFBool).Value;
end;

procedure TAbstractBindableNode.SetBound(const Value: Boolean);
begin
  EventSet_bind.Send(Value);
end;

procedure TAbstractSensorNode.CreateNode;
begin
  inherited;

  FFdEnabled := TSFBool.Create(Self, true, 'enabled', true);
  AddField(FFdEnabled);

  FEventIsActive := TSFBoolEvent.Create(Self, 'isActive', false);
  AddEvent(FEventIsActive);
end;

procedure TMetadataBooleanNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdReference := TSFString.Create(Self, true, 'reference', '');
  AddField(FFdReference);

  FFdValue := TMFBool.Create(Self, true, 'value', []);
  AddField(FFdValue);

  DefaultContainerField := 'metadata';
end;

class function TMetadataBooleanNode.ClassX3DType: string;
begin
  Result := 'MetadataBoolean';
end;

class function TMetadataBooleanNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TMetadataDoubleNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdReference := TSFString.Create(Self, true, 'reference', '');
  AddField(FFdReference);

  FFdValue := TMFDouble.Create(Self, true, 'value', []);
  AddField(FFdValue);

  DefaultContainerField := 'metadata';
end;

class function TMetadataDoubleNode.ClassX3DType: string;
begin
  Result := 'MetadataDouble';
end;

class function TMetadataDoubleNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TMetadataFloatNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdReference := TSFString.Create(Self, true, 'reference', '');
  AddField(FFdReference);

  FFdValue := TMFFloat.Create(Self, true, 'value', []);
  AddField(FFdValue);

  DefaultContainerField := 'metadata';
end;

class function TMetadataFloatNode.ClassX3DType: string;
begin
  Result := 'MetadataFloat';
end;

class function TMetadataFloatNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TMetadataIntegerNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdReference := TSFString.Create(Self, true, 'reference', '');
  AddField(FFdReference);

  FFdValue := TMFInt32.Create(Self, true, 'value', []);
  AddField(FFdValue);

  DefaultContainerField := 'metadata';
end;

class function TMetadataIntegerNode.ClassX3DType: string;
begin
  Result := 'MetadataInteger';
end;

class function TMetadataIntegerNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TMetadataSetNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdReference := TSFString.Create(Self, true, 'reference', '');
  AddField(FFdReference);

  FFdValue := TMFNode.Create(Self, true, 'value', IAbstractMetadataNode);
  AddField(FFdValue);

  DefaultContainerField := 'metadata';
end;

class function TMetadataSetNode.ClassX3DType: string;
begin
  Result := 'MetadataSet';
end;

class function TMetadataSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TMetadataStringNode.CreateNode;
begin
  inherited;

  FFdName := TSFString.Create(Self, true, 'name', '');
  AddField(FFdName);

  FFdReference := TSFString.Create(Self, true, 'reference', '');
  AddField(FFdReference);

  FFdValue := TMFString.Create(Self, true, 'value', []);
  AddField(FFdValue);

  DefaultContainerField := 'metadata';
end;

class function TMetadataStringNode.ClassX3DType: string;
begin
  Result := 'MetadataString';
end;

class function TMetadataStringNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TWorldInfoNode.CreateNode;
begin
  inherited;

  FFdInfo := TMFString.Create(Self, false, 'info', []);
  AddField(FFdInfo);

  FFdTitle := TSFString.Create(Self, false, 'title', '');
  AddField(FFdTitle);
end;

class function TWorldInfoNode.ClassX3DType: string;
begin
  Result := 'WorldInfo';
end;

class function TWorldInfoNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassX3DType) or
    (URN = URNX3DNodes + ClassX3DType);
end;

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
