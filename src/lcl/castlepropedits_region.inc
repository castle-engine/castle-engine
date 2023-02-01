{
  Copyright 2023 Freedomax.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
  {%MainUnit castlepropedits.pas}

  { Region editor. }

  { TCastleRegionEditor ------------------------------------------------- }

type
  TCastleRegionEditor = class(TSubPropertiesEditor)
  public
    function GetName: shortstring; override;
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function TCastleRegionEditor.GetName: shortstring;
begin
  Result:=inherited GetName;
  Result := SuffixRemove('persistent', Result, true);
end;

function TCastleRegionEditor.GetValue: string;
begin
  Result := '(TRegion)';
end;

function TCastleRegionEditor.GetAttributes: TPropertyAttributes;
begin
  Result := (inherited GetAttributes) + [paDialog];
end;

procedure TCastleRegionEditor.Edit;
var
  D: TRegionDesignDialog;
  comp: TPersistent;
  ImagePersistent: TCastleImagePersistent;
begin
  ImagePersistent := nil;

  comp := GetComponent(0);
  if (comp is TCastleImagePersistent) then
    ImagePersistent := comp as TCastleImagePersistent
  else if (comp is TCastleImageControl) then
    ImagePersistent := (comp as TCastleImageControl).Content;

  if not Assigned(ImagePersistent) then
  begin
    ShowMessage('ImagePersistent does not exist.');
    Exit;
  end;
  if (not Assigned(ImagePersistent.DrawableImage)) or
    (ImagePersistent.DrawableImage.Rect.IsEmpty) then
  begin
    ShowMessage('Please load the image first.');
    Exit;
  end;

  D := TRegionDesignDialog.Create(Application);
  try
    D.Caption := 'Edit ' + 'Region';
    D.Image := ImagePersistent.DrawableImage;
    D.Region := ImagePersistent.Region;

    if D.ShowModal = mrOk then
    begin
      D.ApplyChange;
      ImagePersistent.Region := D.Region;
      self.Modified('');
    end;

  finally
    FreeAndNil(D);
  end;
end;
