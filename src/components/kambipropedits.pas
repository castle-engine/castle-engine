{
  Copyright 2010-2011 Michalis Kamburelis.

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
unit KambiPropEdits;

interface

procedure Register;

implementation

uses VRMLScene, PropEdits, KambiLCLUtils, X3DLoad, UIControls,
  KambiGLControl, GLControls, Images, LResources;

type
  TVRMLSceneFileNamePropertyEditor = class(TFileNamePropertyEditor)
  public
    function GetFilter: String; override;
  end;

  TImageFileNamePropertyEditor = class(TFileNamePropertyEditor)
  public
    function GetFilter: String; override;
  end;

  TUIControlListPropertyEditor = class(TListPropertyEditor)
  end;

function TVRMLSceneFileNamePropertyEditor.GetFilter: String;
var
  LCLFilter: string;
  FilterIndex: Integer;
begin
  { TODO: use LoadVRML_FileFilters without "All Files" part. }
  FileFiltersToOpenDialog(LoadVRML_FileFilters, LCLFilter, FilterIndex);
  Result := LCLFilter + (inherited GetFilter);
end;

function TImageFileNamePropertyEditor.GetFilter: String;
var
  LCLFilter: string;
  FilterIndex: Integer;
begin
  { TODO: use LoadImage_FileFilters without "All Files" part. }
  FileFiltersToOpenDialog(LoadImage_FileFilters, LCLFilter, FilterIndex);
  Result := LCLFilter + (inherited GetFilter);
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(AnsiString), TVRMLScene,
    'FileName', TVRMLSceneFileNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TKamGLImage,
    'FileName', TImageFileNamePropertyEditor);
  { TODO: crashes
  RegisterPropertyEditor(TypeInfo(TUIControlList), TKamOpenGLControl,
    'Controls', TUIControlListPropertyEditor);
  }
end;

initialization
  { Add lrs with icons, following
    http://wiki.lazarus.freepascal.org/Lazarus_Packages#Add_a_component_icon }
  {$I icons/kambiicons.lrs}
end.
