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
    constructor Create;
    destructor Destroy; override;
    procedure Assign(const Source: TSlotsAnimated);
  end;

  TSlot = class
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
    Node: TSwitchNode;
    NodeUsedAsChild: boolean;
    ResetAttachment: TIntegerSequencerNode;
    ResetColor1: TColorInterpolatorNode;
    ResetColor2: TScalarInterpolatorNode;
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
end;

destructor TSlotsAnimated.Destroy;
begin
  FreeAndNil(Attachment);
  FreeAndNil(Color);
  inherited;
end;

procedure TSlotsAnimated.Assign(const Source: TSlotsAnimated);
begin
  Attachment.Assign(Source.Attachment);
  Color.Assign(Source.Color);
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
    ResetAttachment.FdKeyValue.Items.Add(Node.FdWhichChoice.Value);
    ContainerForResetNode.FdChildren.Add(ResetAttachment);

    Route := TX3DRoute.Create;
    Route.SetSourceDirectly(ResetAttachment.EventValue_changed);
    Route.SetDestinationDirectly(Node.FdWhichChoice.EventIn);
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

  { TransformNode for a single AttachmentName. }
  function NodeTransform(const AttachmentName: string): TTransformNode;
  const
    DrawOrderZ = 0.01;
  var
    A: TAttachment;
  begin
    Result := TTransformNode.Create('SlotAttachment_' + ToX3DName(Name) + '_' + ToX3DName(AttachmentName), BaseUrl);
    Result.FdTranslation.Value := Vector3Single(0, 0, DrawOrder * DrawOrderZ);

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
        Result.FdChildren.Add(A.Node);
        Materials.Add(A.Material);
      end;
    end;
  end;

var
  I: Integer;
begin
  Node := TSwitchNode.Create('Slot_' + ToX3DName(Name), BaseUrl);
  Node.FdWhichChoice.Value := 0;
  NodeUsedAsChild := true;
  Bone.Node.FdChildren.Add(Node);

  Materials.Clear;

  for I := 0 to AttachmentNames.Count - 1 do
    Node.FdChildren.Add(NodeTransform(AttachmentNames[I]));

  AddResetAttachment;
  AddResetColor;
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
