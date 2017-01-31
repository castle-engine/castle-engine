{
  Copyright 2010-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Property editors for Lazarus components.
  The main source of information about this is in comments of Lazarus
  ideintf/propedits.pp.

  @exclude (This unit is not supposed to be normally used, so not documented
  by PasDoc. It's only for Lazarus registration.) }
unit CastlePropEdits;

{$I castleconf.inc}

interface

procedure Register;

implementation

uses CastleSceneCore, PropEdits, CastleLCLUtils, X3DLoad, CastleUIControls,
  CastleControl, CastleControls, CastleImages, LResources;

type
  TSceneURLPropertyEditor = class(TFileNamePropertyEditor)
  public
    function GetFilter: String; override;
  end;

  TImageURLPropertyEditor = class(TFileNamePropertyEditor)
  public
    function GetFilter: String; override;
  end;

  TChildrenControlsPropertyEditor = class(TListPropertyEditor)
  end;

function TSceneURLPropertyEditor.GetFilter: String;
var
  LCLFilter: string;
  FilterIndex: Integer;
begin
  { TODO: use Load3D_FileFilters without "All Files" part. }
  FileFiltersToDialog(Load3D_FileFilters, LCLFilter, FilterIndex);
  Result := LCLFilter + (inherited GetFilter);
end;

function TImageURLPropertyEditor.GetFilter: String;
var
  LCLFilter: string;
  FilterIndex: Integer;
begin
  { TODO: use LoadImage_FileFilters without "All Files" part. }
  FileFiltersToDialog(LoadImage_FileFilters, LCLFilter, FilterIndex);
  Result := LCLFilter + (inherited GetFilter);
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleSceneCore,
    'URL', TSceneURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleImageControl,
    'URL', TImageURLPropertyEditor);
  { TODO: crashes
  RegisterPropertyEditor(TypeInfo(TChildrenControls), TCastleControlCustom,
    'Controls', TChildrenControlsPropertyEditor);
  }
end;

initialization
  { Add lrs with icons, following
    http://wiki.lazarus.freepascal.org/Lazarus_Packages#Add_a_component_icon }
  {$I icons/castleicons.lrs}
end.
