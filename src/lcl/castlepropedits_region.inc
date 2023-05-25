{%MainUnit castlepropedits.pas}
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

{ Region editor. }

{ TCastleRegionEditor ------------------------------------------------- }

type
  { Edit either TFloatRectanglePersistent or TBorder (when used for 9-slices). }
  TCastleRegionEditor = class(TSubPropertiesEditor)
  public
    function GetName: shortstring; override;
    function GetValue: String; override;
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function TCastleRegionEditor.GetName: shortstring;
begin
  Result := inherited GetName;
  Result := SuffixRemove('persistent', Result, True);
end;

function TCastleRegionEditor.GetValue: String;
var
  VClassName: String;
  VName: String;
begin
  VName := GetName;
  if VName = 'Region' then
    VClassName := 'TFloatRectangle'
  else
    VClassName := GetPropInfo^.PropType^.Name;
  Result := '(' + SuffixRemove('persistent', VClassName, True) + ')';
  if VName = 'ProtectedSides' then
    Result := Result + ' (9-slice scaling)';
end;

function TCastleRegionEditor.GetAttributes: TPropertyAttributes;
begin
  Result := (inherited GetAttributes) + [paDialog];
end;

procedure TCastleRegionEditor.Edit;
var
  D: TRegionDesignDialog;
  Comp: TPersistent;
  ImagePersistent: TCastleImagePersistent;
  VClassName: String;
begin
  ImagePersistent := nil;

  Comp := GetComponent(0);
  if (Comp is TCastleImagePersistent) then
    ImagePersistent := Comp as TCastleImagePersistent
  else
  if (Comp is TCastleImageControl) then
    ImagePersistent := (Comp as TCastleImageControl).Content;

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

  VClassName := GetPropInfo^.PropType^.Name;

  D := TRegionDesignDialog.Create(Application);
  try
    if VClassName = TFloatRectanglePersistent.ClassName then
    begin
      D.Caption := 'Edit Region';
      D.Load(ImagePersistent.DrawableImage, ImagePersistent.Region);
    end
    else
    begin
      D.Caption := 'Edit Border';
      D.Load(ImagePersistent.DrawableImage, ImagePersistent.Region,
        ImagePersistent.ProtectedSides);
    end;

    if D.ShowModal = mrOk then
    begin
      D.ApplyChange;

      case D.DesignMode of
        dmRegion:
          ImagePersistent.Region := D.Region;
        dmBorder:
          D.ApplyToBorder(ImagePersistent.ProtectedSides);
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('D.DesignMode?');
        {$endif}
      end;

      Modified('');
    end;

  finally
    FreeAndNil(D);
  end;
end;
