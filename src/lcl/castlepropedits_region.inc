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
    function GetValue: string; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
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

  if not Assigned(ImagePersistent) then Exit;

  D := TRegionDesignDialog.Create(Application);
  try
    D.Caption := 'Edit ' + 'Region';

    D.Image := ImagePersistent.DrawableImage;

    if Assigned(D.Image) then
    begin
      D.Region := ImagePersistent.Region;

      if D.ShowModal = mrOk then
      begin
        D.ApplyChange;
        self.Modified('');
      end;
    end
    else
      ShowMessage('Please load the image first.');

  finally
    FreeAndNil(D);
  end;
end;
