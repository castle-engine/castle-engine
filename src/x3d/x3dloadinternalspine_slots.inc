{
  Copyright 2014-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Spine slots. }

{$ifdef read_interface}
  TSlotList = class;

  TMaterialNodeList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TMaterialNode>;

  TSlotsAnimated = class
    Attachment: TSlotList;
    Color: TSlotList;
    DrawOrder: TSlotList;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const Source: TSlotsAnimated);
  end;

  TColorInterpolatorNodeList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TColorInterpolatorNode>;
  TScalarInterpolatorNodeList = {$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TScalarInterpolatorNode>;

  TSlot = class
  public
  const
    DrawOrderZ = 0.01;
  var
    Name: string;
    Bone: TBone;
    Color: TCastleColor;
    { Possible attachment names on this slot.
      First attachment name (alwats exists, but may be '')
      indicates the setup pose attachment.
      The rest of attachments may be used during animations.
      The order of this list is also the order of Switch @link(Node),
      that represents this slot in X3D. }
    AttachmentNames: TStringList;
    { Draw order, from 1st (most background) to last (most foreground).
      Initially derived looking at slots order, this is what determines
      drawing order for spine, see
      http://esotericsoftware.com/spine-using-runtimes }
    DrawOrder: Integer;
    Node: TTransformNode;
    SwitchNode: TSwitchNode;
    NodeUsedAsChild: boolean;
    ResetAttachment: TIntegerSequencerNode;
    ResetColor1: TColorInterpolatorNodeList;
    ResetColor2: TScalarInterpolatorNodeList;
    ResetDrawOrder: TPositionInterpolatorNode;
    Materials: TMaterialNodeList;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList;
      const DragonBonesFormat: boolean);
    procedure BuildNodes(const BaseUrl: string;
      const SlotsAnimated: TSlotsAnimated;
      const AttachmentsPreferred, AttachmentsDefault: TAttachmentList;
      const ContainerForResetNode: TAbstractX3DGroupingNode);
  end;

  TSlotList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TSlot>)
    { Find slot by name.
      @raises ESpineReadError If slot does not exist. }
    function Find(const SlotName: string): TSlot;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList;
      const DragonBonesFormat: boolean);
    procedure BuildNodes(const BaseUrl: string;
      const SlotsAnimated: TSlotsAnimated;
      const AttachmentsPreferred, AttachmentsDefault: TAttachmentList;
      const ContainerForResetNode: TAbstractX3DGroupingNode);
    procedure Assign(const Source: TSlotList);
  end;
{$endif}

{$ifdef read_implementation}

{ TSlotsAnimated ------------------------------------------------------------- }

constructor TSlotsAnimated.Create;
begin
  inherited;
  Attachment := TSlotList.Create(false);
  Color := TSlotList.Create(false);
  DrawOrder := TSlotList.Create(false);
end;

destructor TSlotsAnimated.Destroy;
begin
  FreeAndNil(Attachment);
  FreeAndNil(Color);
  FreeAndNil(DrawOrder);
  inherited;
end;

procedure TSlotsAnimated.Assign(const Source: TSlotsAnimated);
begin
  Attachment.Assign(Source.Attachment);
  Color.Assign(Source.Color);
  DrawOrder.Assign(Source.DrawOrder);
end;

{ TSlot ---------------------------------------------------------------------- }

constructor TSlot.Create;
begin
  inherited;
  AttachmentNames := TStringList.Create;
  Materials := TMaterialNodeList.Create(false);
  ResetColor1 := TColorInterpolatorNodeList.Create(false);
  ResetColor2 := TScalarInterpolatorNodeList.Create(false);
end;

destructor TSlot.Destroy;
begin
  if NodeUsedAsChild then
    Node := nil
  else
    FreeIfUnusedAndNil(Node);
  FreeAndNil(AttachmentNames);
  FreeAndNil(Materials);
  FreeAndNil(ResetColor1);
  FreeAndNil(ResetColor2);
  inherited;
end;

procedure TSlot.Parse(const Json: TJSONObject; const Bones: TBoneList;
  const DragonBonesFormat: boolean);
begin
  Name := Json.Get('name', '');
  if DragonBonesFormat then
    Bone := Bones.Find(Json.Get('parent', ''))
  else
    Bone := Bones.Find(Json.Get('bone', ''));
  AttachmentNames.Add(Json.Get('attachment', ''));
  Color := HexToColor(Json.Get('color', 'ffffffff'));
end;

procedure TSlot.BuildNodes(const BaseUrl: string;
  const SlotsAnimated: TSlotsAnimated;
  const AttachmentsPreferred, AttachmentsDefault: TAttachmentList;
  const ContainerForResetNode: TAbstractX3DGroupingNode);

  { Prepare interpolators that allow to reset bone to initial transformation
    after some animations. }

  procedure AddResetAttachment;
  var
    Route: TX3DRoute;
  begin
    if SlotsAnimated.Attachment.IndexOf(Self) = -1 then Exit;

    ResetAttachment := TIntegerSequencerNode.Create('SlotReset_attachment_' + Name);
    ResetAttachment.FdForceContinousValue_Changed.Value := true;
    ResetAttachment.FdKey.Items.Add(0);
    ResetAttachment.FdKeyValue.Items.Add(SwitchNode.FdWhichChoice.Value);
    ContainerForResetNode.AddChildren(ResetAttachment);

    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(ResetAttachment.EventValue_changed);
    Route.SetDestinationDirectly(SwitchNode.FdWhichChoice.EventIn);
    ContainerForResetNode.AddRoute(Route);
  end;

  procedure AddResetColor;
  var
    Route: TX3DRoute;
    I: Integer;
    ColInterp: TColorInterpolatorNode;
    ScalarInterp: TScalarInterpolatorNode;
  begin
    if SlotsAnimated.Color.IndexOf(Self) = -1 then Exit;

    for I := 0 to Materials.Count - 1 do
    begin
      ColInterp := TColorInterpolatorNode.Create('SlotReset_color_' + Name);
      ColInterp.FdKey.Items.Add(0);
      ColInterp.FdKeyValue.Items.Add(Color.XYZ);
      ContainerForResetNode.AddChildren(ColInterp);
      ResetColor1.Add(ColInterp);

      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(ColInterp.EventValue_changed);
      Route.SetDestinationDirectly(Materials[I].FdEmissiveColor);
      ContainerForResetNode.AddRoute(Route);

      ScalarInterp := TScalarInterpolatorNode.Create('SlotReset_alpha_' + Name);
      ScalarInterp.FdKey.Items.Add(0);
      ScalarInterp.FdKeyValue.Items.Add(1 - Color[3]);
      ContainerForResetNode.AddChildren(ScalarInterp);
      ResetColor2.Add(ScalarInterp);

      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(ScalarInterp.EventValue_changed);
      Route.SetDestinationDirectly(Materials[I].FdTransparency);
      ContainerForResetNode.AddRoute(Route);
    end;
  end;

  procedure AddResetDrawOrder;
  var
    Route: TX3DRoute;
  begin
    if SlotsAnimated.DrawOrder.IndexOf(Self) = -1 then Exit;

    ResetDrawOrder := TPositionInterpolatorNode.Create('SlotReset_draworder_' + Name);
    ResetDrawOrder.FdKey.Items.Add(0);
    ResetDrawOrder.FdKeyValue.Items.Add(Node.FdTranslation.Value);
    ContainerForResetNode.AddChildren(ResetDrawOrder);

    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(ResetDrawOrder.EventValue_changed);
    Route.SetDestinationDirectly(Node.FdTranslation.EventIn);
    ContainerForResetNode.AddRoute(Route);
  end;

  { Node for a single AttachmentName. Never @nil. }
  function AttachmentNode(const AttachmentName: string): TAbstractChildNode;
  var
    A: TAttachment;
  begin
    Result := nil;

    { ignore empty attachment names, as http://esotericsoftware.com/spine-json-format
      says explicitly "Assume no attachment for the setup pose if omitted." }
    if AttachmentName <> '' then
    begin
      if AttachmentsPreferred <> AttachmentsDefault then
        A := AttachmentsPreferred.Find(Name, AttachmentName, AttachmentsDefault, true) else
        A := AttachmentsPreferred.Find(Name, AttachmentName, nil, true);
      if A <> nil then
      begin
        A.TransformToBoneSpace(Bone); // transform skinnedmesh into local space
        A.NodeUsedAsChild := true;
        Result := A.Node;
        Materials.Add(A.Material);
      end;
    end;

    { the caller needs non-nil result, to have 1 node for every AttachmentName,
      so that Switch.whichChoice can work reliably.
      So create an empty node if needed. }
    if Result = nil then
      Result := TStaticGroupNode.Create('SlotEmptyAttachment_' + Name, BaseUrl);
  end;

var
  I: Integer;
begin
  Node := TTransformNode.Create('SlotTransform_' + Name, BaseUrl);
  Node.FdTranslation.Value := Vector3(0, 0, DrawOrder * DrawOrderZ);
  Bone.Node.AddChildren(Node);
  NodeUsedAsChild := true;

  SwitchNode := TSwitchNode.Create('SlotSwitch_' + Name, BaseUrl);
  SwitchNode.FdWhichChoice.Value := 0;
  Node.AddChildren(SwitchNode);

  Materials.Clear;

  for I := 0 to AttachmentNames.Count - 1 do
    SwitchNode.AddChildren(AttachmentNode(AttachmentNames[I]));

  AddResetAttachment;
  AddResetColor;
  AddResetDrawOrder;
end;

{ TSlotList ------------------------------------------------------------------ }

function TSlotList.Find(const SlotName: string): TSlot;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
    if Items[Index].Name = SlotName then
      Exit(Items[Index]);
  raise ESpineReadError.CreateFmt('Slot name "%s" not found', [SlotName]);
end;

procedure TSlotList.Parse(const Json: TJSONObject; const Bones: TBoneList; const DragonBonesFormat: boolean);
var
  I: Integer;
  Slot: TSlot;
  ChildArray: TJSONArray;
  ArrayName: string;
begin
  if DragonBonesFormat then
    ArrayName := 'slot'
  else
    ArrayName := 'slots';
  ChildArray := Json.Find(ArrayName, jtArray) as TJSONArray;
  if ChildArray = nil then
    raise ESpineReadError.CreateFmt('Spine / Dragon Bones JSON skeleton: Missing "%s" array', [ArrayName]);

  for I := 0 to ChildArray.Count - 1 do
    if ChildArray[I] is TJSONObject then
    begin
      Slot := TSlot.Create;
      Slot.DrawOrder := Count;
      Add(Slot);
      Slot.Parse(TJSONObject(ChildArray[I]), Bones, DragonBonesFormat);
    end;
end;

procedure TSlotList.BuildNodes(const BaseUrl: string;
  const SlotsAnimated: TSlotsAnimated;
  const AttachmentsPreferred, AttachmentsDefault: TAttachmentList;
  const ContainerForResetNode: TAbstractX3DGroupingNode);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].BuildNodes(BaseUrl, SlotsAnimated,
      AttachmentsPreferred, AttachmentsDefault, ContainerForResetNode);
end;

procedure TSlotList.Assign(const Source: TSlotList);
begin
  Clear;
  AddRange(Source);
end;

{$endif}
