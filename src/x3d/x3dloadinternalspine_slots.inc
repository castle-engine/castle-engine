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
  TSlot = class
    Name: string;
    Bone: TBone;
    Color: TCastleColor;
    Attachment: string;
    { Draw order, from 1st (most background) to last (most foreground).
      Initially derived looking at slots order, this is what determines
      drawing order for spine, see
      http://esotericsoftware.com/spine-using-runtimes }
    DrawOrder: Integer;
    Node: TTransformNode;
    NodeUsedAsChild: boolean;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList);
    procedure BuildNodes(const BaseUrl: string;
      const AttachmentsPreferred, AttachmentsDefault: TAttachmentList);
  end;

  TSlotList = class(specialize TFPGObjectList<TSlot>)
    procedure Parse(const Json: TJSONObject; const Bones: TBoneList);
    procedure BuildNodes(const BaseUrl: string;
      const AttachmentsPreferred, AttachmentsDefault: TAttachmentList);
  end;
{$endif}

{$ifdef read_implementation}

{ TSlot ---------------------------------------------------------------------- }

procedure TSlot.Parse(const Json: TJSONObject; const Bones: TBoneList);
begin
  Name := Json.Get('name', '');
  Bone := Bones.Find(Json.Get('bone', ''));
  Attachment := Json.Get('attachment', '');
  // TODO: Color :=
end;

procedure TSlot.BuildNodes(const BaseUrl: string;
  const AttachmentsPreferred, AttachmentsDefault: TAttachmentList);
const
  DrawOrderZ = 0.01;
var
  A: TAttachment;
begin
  { ignore empty attachment names, as http://esotericsoftware.com/spine-json-format
    says explicitly "Assume no attachment for the setup pose if omitted." }
  if Attachment <> '' then
  begin
    Node := TTransformNode.Create('Slot_' + ToX3DName(Name), BaseUrl);
    Node.FdTranslation.Value := Vector3Single(0, 0, DrawOrder * DrawOrderZ);
    NodeUsedAsChild := true;
    Bone.Node.FdChildren.Add(Node);

    if AttachmentsPreferred <> AttachmentsDefault then
      A := AttachmentsPreferred.Find(Name, Attachment, AttachmentsDefault) else
      A := AttachmentsPreferred.Find(Name, Attachment, nil);
    A.NodeUsedAsChild := true;
    Node.FdChildren.Add(A.Node);
  end;
end;

destructor TSlot.Destroy;
begin
  if NodeUsedAsChild then
    Node := nil else
    FreeIfUnusedAndNil(Node);
  inherited;
end;

{ TSlotList ------------------------------------------------------------------ }

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
  const AttachmentsPreferred, AttachmentsDefault: TAttachmentList);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].BuildNodes(BaseUrl, AttachmentsPreferred, AttachmentsDefault);
end;

{$endif}
