{
  Copyright 2010-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Property editors for Lazarus components.
  The main source of information about this is in comments of Lazarus
  ideintf/propedits.pp. }
unit KambiPropEdits;

interface

procedure Register;

implementation

uses VRMLScene, PropEdits, KambiLCLUtils, Object3DAsVRML, UIControls,
  KambiGLControl;

type
  TVRMLSceneFileNamePropertyEditor = class(TFileNamePropertyEditor)
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
  { TODO: use LoadAsVRML_FileFilters without "All Files" part. }
  FileFiltersToOpenDialog(LoadAsVRML_FileFilters, LCLFilter, FilterIndex);
  Result := LCLFilter + (inherited GetFilter);
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(AnsiString), TVRMLScene,
    'FileName', TVRMLSceneFileNamePropertyEditor);
  { TODO: crashes
  RegisterPropertyEditor(TypeInfo(TUIControlList), TKamOpenGLControl,
    'Controls', TUIControlListPropertyEditor);
  }
end;

end.
