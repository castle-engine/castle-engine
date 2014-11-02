{
  Copyright 2014-2014 Michalis Kamburelis.

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

  TMaterialNodeList = specialize TFPGObjectList<TMaterialNode>;

  TSlotsAnimated = class
    Attachment: TSlotList;
    Color: TSlotList;
    DrawOrder: TSlotList;
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const Source: TSlotsAnimated);
  end;

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
    ResetColor1: TColorInterpolatorNode;
    ResetColor2: TScalarInterpolatorNode;
    ResetDrawOrder: TPositionInterpolatorNode;
    Materials: TMaterialNodeList;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList);
    procedure BuildNodes(const BaseUrl: string;
      const SlotsAnimated: TSlotsAnimated;
      const AttachmentsPreferred, AttachmentsDefault: TAttachmentList;
      const ContainerForResetNode: TAbstractX3DGroupingNode);
  end;

  TSlotList = class(specialize TFPGObjectList<TSlot>)
    { Find slot by name.
      @raises ESpineReadError If slot does not exist. }
    function Find(const SlotName: string): TSlot;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList);
    procedure BuildNodes(const BaseUrl: string;
      const SlotsAnimated: TSlotsAnimated;
      const AttachmentsPreferred, AttachmentsDefault: TAttachmentList;
      const ContainerForResetNode: TAbstractX3DGroupingNode);
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
end;

destructor TSlot.Destroy;
begin
  if NodeUsedAsChild then
    Node := nil else
    FreeIfUnusedAndNil(Node);
  FreeAndNil(AttachmentNames);
  FreeAndNil(Materials);
  inherited;
end;

procedure TSlot.Parse(const Json: TJSONObject; const Bones: TBoneList);
begin
  Name := Json.Get('name', '');
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

    ResetAttachment := TIntegerSequencerNode.Create('SlotReset_attachment_' + ToX3DName(Name));
    ResetAttachment.FdForceContinousValue_Changed.Value := true;
    ResetAttachment.FdKey.Items.Add(0);
    ResetAttachment.FdKeyValue.Items.Add(SwitchNode.FdWhichChoice.Value);
    ContainerForResetNode.FdChildren.Add(ResetAttachment);

    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(ResetAttachment.EventValue_changed);
    Route.SetDestinationDirectly(SwitchNode.FdWhichChoice.EventIn);
    ContainerForResetNode.Routes.Add(Route);
  end;

  procedure AddResetColor;
  var
    Route: TX3DRoute;
    I: Integer;
  begin
    if SlotsAnimated.Color.IndexOf(Self) = -1 then Exit;

    for I := 0 to Materials.Count - 1 do
    begin
      ResetColor1 := TColorInterpolatorNode.Create('SlotReset_color_' + ToX3DName(Name));
      ResetColor1.FdKey.Items.Add(0);
      ResetColor1.FdKeyValue.Items.Add(Vector3SingleCut(Color));
      ContainerForResetNode.FdChildren.Add(ResetColor1);

      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(ResetColor1.EventValue_changed);
      Route.SetDestinationDirectly(Materials[I].FdEmissiveColor);
      ContainerForResetNode.Routes.Add(Route);

      ResetColor2 := TScalarInterpolatorNode.Create('SlotReset_alpha_' + ToX3DName(Name));
      ResetColor2.FdKey.Items.Add(0);
      ResetColor2.FdKeyValue.Items.Add(1 - Color[3]);
      ContainerForResetNode.FdChildren.Add(ResetColor2);

      Route := TX3DRoute.Create;
      Route.SetSourceDirectly(ResetColor2.EventValue_changed);
      Route.SetDestinationDirectly(Materials[I].FdTransparency);
      ContainerForResetNode.Routes.Add(Route);
    end;
  end;

  procedure AddResetDrawOrder;
  var
    Route: TX3DRoute;
  begin
    if SlotsAnimated.DrawOrder.IndexOf(Self) = -1 then Exit;

    ResetDrawOrder := TPositionInterpolatorNode.Create('SlotReset_draworder_' + ToX3DName(Name));
    ResetDrawOrder.FdKey.Items.Add(0);
    ResetDrawOrder.FdKeyValue.Items.Add(Node.FdTranslation.Value);
    ContainerForResetNode.FdChildren.Add(ResetDrawOrder);

    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(ResetDrawOrder.EventValue_changed);
    Route.SetDestinationDirectly(Node.FdTranslation.EventIn);
    ContainerForResetNode.Routes.Add(Route);
  end;

  { Node for a single AttachmentName. Never @nil. }
  function AttachmentNode(const AttachmentName: string): TX3DNode;
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
        A.NodeUsedAsChild := true;
        Result := A.Node;
        Materials.Add(A.Material);
      end;
    end;

    { the caller needs non-nil result, to have 1 node for every AttachmentName,
      so that Switch.whichChoice can work reliably.
      So create an empty node if needed. }
    if Result = nil then
      Result := TStaticGroupNode.Create('', BaseUrl);
  end;

var
  I: Integer;
begin
  Node := TTransformNode.Create('SlotTransform_' + ToX3DName(Name), BaseUrl);
  Node.FdTranslation.Value := Vector3Single(0, 0, DrawOrder * DrawOrderZ);
  Bone.Node.FdChildren.Add(Node);
  NodeUsedAsChild := true;

  SwitchNode := TSwitchNode.Create('SlotSwitch_' + ToX3DName(Name), BaseUrl);
  SwitchNode.FdWhichChoice.Value := 0;
  Node.FdChildren.Add(SwitchNode);

  Materials.Clear;

  for I := 0 to AttachmentNames.Count - 1 do
    SwitchNode.FdChildren.Add(AttachmentNode(AttachmentNames[I]));

  AddResetAttachment;
  AddResetColor;
  AddResetDrawOrder;
end;

{ TSlotList ------------------------------------------------------------------ }

function TSlotList.Find(const SlotName: string): TSlot;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = SlotName then
      Exit(Items[I]);
  raise ESpineReadError.CreateFmt('Slot name "%s" not found', [SlotName]);
end;

procedure TSlotList.Parse(const Json: TJSONObject; const Bones: TBoneList);
var
  I: Integer;
  Slot: TSlot;
  ChildArray: TJSONArray;
begin
  ChildArray := Json.Find('slots', jtArray) as TJSONArray;
  if ChildArray = nil then
    raise ESpineReadError.Create('Spine JSON skeleton: Missing "slots" array');

  for I := 0 to ChildArray.Count - 1 do
    if ChildArray[I] is TJSONObject then
    begin
      Slot := TSlot.Create;
      Slot.DrawOrder := Count;
      Add(Slot);
      Slot.Parse(TJSONObject(ChildArray[I]), Bones);
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

{$endif}
