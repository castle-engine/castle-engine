{%MainUnit x3dloadinternalspine.pas}
{
  Copyright 2014-2023 Michalis Kamburelis.

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

  TUnlitMaterialNodeList = {$ifdef FPC}specialize{$endif} TObjectList<TUnlitMaterialNode>;

  TColorInterpolatorNodeList = {$ifdef FPC}specialize{$endif} TObjectList<TColorInterpolatorNode>;
  TScalarInterpolatorNodeList = {$ifdef FPC}specialize{$endif} TObjectList<TScalarInterpolatorNode>;

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
    Materials: TUnlitMaterialNodeList;
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList;
      const DragonBonesFormat: boolean);
    procedure BuildNodes(const BaseUrl: string;
      const AttachmentsPreferred, AttachmentsDefault: TAttachmentList);
  end;

  TSlotList = class({$ifdef FPC}specialize{$endif} TObjectList<TSlot>)
    { Find slot by name.
      @raises ESpineReadError If slot does not exist. }
    function Find(const SlotName: string): TSlot;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList;
      const DragonBonesFormat: boolean);
    procedure BuildNodes(const BaseUrl: string;
      const AttachmentsPreferred, AttachmentsDefault: TAttachmentList);
    procedure Assign(const Source: TSlotList);
  end;
{$endif}

{$ifdef read_implementation}

{ TSlot ---------------------------------------------------------------------- }

constructor TSlot.Create;
begin
  inherited;
  AttachmentNames := TStringList.Create;
  Materials := TUnlitMaterialNodeList.Create(false);
end;

destructor TSlot.Destroy;
begin
  if NodeUsedAsChild then
    Node := nil
  else
    FreeIfUnusedAndNil(Node);
  FreeAndNil(AttachmentNames);
  FreeAndNil(Materials);
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
  const AttachmentsPreferred, AttachmentsDefault: TAttachmentList);
var
  IsWeightedMeshAttachmentExisted: Boolean {$ifdef FPC}= False{$endif};
  IsOtherAttachmentExisted: Boolean {$ifdef FPC}= False{$endif};

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
        if A.NodeUsedAsChild and
           not (
             TVector3.PerfectlyEquals(A.Material.FdEmissiveColor.Value, WhiteRGB) or
             (A.Material.FdTransparency.Value <> 1.0) or
             TVector4.PerfectlyEquals(Color, White)
           ) then
        begin
          WritelnWarning('Spine', 'Attachment "%s" used by multiple slots with different (not opaque white) colors. This is not supported, instead make the slot colors to be applied during animations.',
            [A.Node.X3DName]);
        end;

        { TODO: This is a hackjob and need a better way to handle this issue.
          Since we "baked" mesh transformation into TCoordinateInterpolatorNode,
          including bone transformation, we need to move this mesh to root bone. }
        if (A is TMeshAttachment) and ((A as TMeshAttachment).Weights.Count > 0) then
        begin
          IsWeightedMeshAttachmentExisted := True;
          while Bone.Parent <> nil do
          begin
            Bone := Bone.Parent;
          end;
        end else
        begin
          IsOtherAttachmentExisted := True;
        end;

        { Transform skinnedmesh into local space.
          Note that the new weighted mesh baked transformations directly into
          TCoordinateInterpolatorNode so this is not necessary for weighted mesh. }
        A.TransformToBoneSpace(Bone);
        A.NodeUsedAsChild := true;
        Result := A.Node;

        // We need to copy slot color to the material node inside attachment
        A.Material.FdEmissiveColor.Value := Color.XYZ;
        A.Material.FdTransparency.Value := 1 - Color[3];

        { Do not put duplicates on Materials list,
          this would cause TSlotTimelineColor.BuildNodes
          to create multiple ROUTEs to the same node. }
        if not Materials.Contains(A.Material) then
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
  {$ifndef FPC}
  // can't initialize local variables in Delphi
  IsWeightedMeshAttachmentExisted := false;
  IsOtherAttachmentExisted := false;
  {$endif}

  Node := TTransformNode.Create('SlotTransform_' + Name, BaseUrl);
  Node.FdTranslation.Value := Vector3(0, 0, DrawOrder * DrawOrderZ);
  NodeUsedAsChild := true;

  SwitchNode := TSwitchNode.Create('SlotSwitch_' + Name, BaseUrl);
  SwitchNode.FdWhichChoice.Value := 0;
  Node.AddChildren(SwitchNode);

  Materials.Clear;

  for I := 0 to AttachmentNames.Count - 1 do
    SwitchNode.AddChildren(AttachmentNode(AttachmentNames[I]));
  if IsWeightedMeshAttachmentExisted and IsOtherAttachmentExisted then
    WritelnWarning('Spine', 'Weighted mesh attachment alongside with other types of attachment in the same slot is not supported');
  Bone.Node.AddChildren(Node);
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
  const AttachmentsPreferred, AttachmentsDefault: TAttachmentList);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].BuildNodes(BaseUrl, AttachmentsPreferred, AttachmentsDefault);
end;

procedure TSlotList.Assign(const Source: TSlotList);
begin
  Clear;
  AddRange(Source);
end;

{$endif}
