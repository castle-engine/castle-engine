{%MainUnit castlepropedits.pas}
{
  Copyright 2010-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ URL editors. }

{$ifdef read_interface}

type
  { Property editor for URL that refers to something that can be loaded by
    @link(LoadNode) or @link(TCastleSceneCore.Load).
    Show an TCastleOpenSceneDialog on Edit. }
  TSceneUrlPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { Property editor for URL that refers to an image readable by Castle Game Engine.
    Show an TCastleOpenImageDialog on Edit. }
  TImageUrlPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { Property editor for URL that refers to a file readable by UserInterfaceLoad. }
  TUiDesignUrlPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { Property editor for URL that refers to a file readable by TransformLoad. }
  TTransformDesignUrlPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { Property editor for URL that refers to a file readable by ComponentLoad. }
  TAnyDesignUrlPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { Property editor for URL that refers to a Tiled Map file. }
  TTiledMapUrlPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { Property editor for URL that refers to a sound file. }
  TSoundUrlPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { Property editor for URL that refers to a font file. }
  TFontUrlPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ Useful by many dialogs here ------------------------------------------------ }

function PropertyEditorsAdviceDataDirectory: Boolean;
begin
  { There's no reason to leave it false, in practice. }
  Result := true;

  if Result then
    { By the way, do FixApplicationDataInIDE to have good ApplicationDataOverride. }
    FixApplicationDataInIDE;
end;

{ TSceneUrlPropertyEditor ---------------------------------------------------- }

function TSceneUrlPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paMultiSelect];
end;

procedure TSceneUrlPropertyEditor.Edit;
var
  Dialog: TCastleOpenSceneDialog;
begin
  Dialog := TCastleOpenSceneDialog.Create(nil);
  try
    Dialog.AdviceDataDirectory := PropertyEditorsAdviceDataDirectory;
    if (GetStrValue = '') and PropertyEditorsAdviceDataDirectory then
      Dialog.URL := 'castle-data:/' // make it easier to open files within data folder
    else
      Dialog.URL := GetStrValue;
    if Dialog.Execute then
      SetStrValue(Dialog.URL);
  finally FreeAndNil(Dialog) end;
end;

{ TImageUrlPropertyEditor ---------------------------------------------------- }

function TImageUrlPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paMultiSelect];
end;

procedure TImageUrlPropertyEditor.Edit;
var
  Dialog: TCastleOpenImageDialog;
begin
  Dialog := TCastleOpenImageDialog.Create(nil);
  try
    Dialog.AdviceDataDirectory := PropertyEditorsAdviceDataDirectory;
    if (GetStrValue = '') and PropertyEditorsAdviceDataDirectory then
      Dialog.URL := 'castle-data:/' // make it easier to open files within data folder
    else
      Dialog.URL := GetStrValue;
    if Dialog.Execute then
      SetStrValue(Dialog.URL);
  finally FreeAndNil(Dialog) end;
end;

{ TUiDesignUrlPropertyEditor ---------------------------------------------------- }

function TUiDesignUrlPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paMultiSelect];
end;

procedure TUiDesignUrlPropertyEditor.Edit;
var
  Dialog: TCastleOpenDialog;
begin
  Dialog := TCastleOpenDialog.Create(nil);
  try
    Dialog.Filter := 'CGE User Interace Design (*.castle-user-interface)|*.castle-user-interface|All Files|*';
    Dialog.AdviceDataDirectory := PropertyEditorsAdviceDataDirectory;
    if (GetStrValue = '') and PropertyEditorsAdviceDataDirectory then
      Dialog.URL := 'castle-data:/' // make it easier to open files within data folder
    else
      Dialog.URL := GetStrValue;
    if Dialog.Execute then
      SetStrValue(Dialog.URL);
  finally FreeAndNil(Dialog) end;
end;

{ TTransformDesignUrlPropertyEditor ---------------------------------------------------- }

function TTransformDesignUrlPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paMultiSelect];
end;

procedure TTransformDesignUrlPropertyEditor.Edit;
var
  Dialog: TCastleOpenDialog;
begin
  Dialog := TCastleOpenDialog.Create(nil);
  try
    Dialog.Filter := 'CGE Transform Design (*.castle-transform)|*.castle-transform|All Files|*';
    Dialog.AdviceDataDirectory := PropertyEditorsAdviceDataDirectory;
    if (GetStrValue = '') and PropertyEditorsAdviceDataDirectory then
      Dialog.URL := 'castle-data:/' // make it easier to open files within data folder
    else
      Dialog.URL := GetStrValue;
    if Dialog.Execute then
      SetStrValue(Dialog.URL);
  finally FreeAndNil(Dialog) end;
end;

{ TAnyDesignUrlPropertyEditor ---------------------------------------------------- }

function TAnyDesignUrlPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paMultiSelect];
end;

procedure TAnyDesignUrlPropertyEditor.Edit;
var
  Dialog: TCastleOpenDialog;
begin
  Dialog := TCastleOpenDialog.Create(nil);
  try
    Dialog.Filter := 
      'CGE Design (*.castle-user-interface, *.castle-transform, *.castle-component)|*.castle-user-interface;*.castle-transform;*.castle-component|' +
      'CGE User Interace Design (*.castle-user-interface)|*.castle-user-interface|' + 
      'CGE Transform Design (*.castle-transform)|*.castle-transform|' +
      'CGE Non-Visual Design (*.castle-component)|*.castle-component|' +      
      'All Files|*';
    Dialog.AdviceDataDirectory := PropertyEditorsAdviceDataDirectory;
    if (GetStrValue = '') and PropertyEditorsAdviceDataDirectory then
      Dialog.URL := 'castle-data:/' // make it easier to open files within data folder
    else
      Dialog.URL := GetStrValue;
    if Dialog.Execute then
      SetStrValue(Dialog.URL);
  finally FreeAndNil(Dialog) end;
end;

{ TTiledMapUrlPropertyEditor ---------------------------------------------------- }

function TTiledMapUrlPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paMultiSelect];
end;

procedure TTiledMapUrlPropertyEditor.Edit;
var
  Dialog: TCastleOpenDialog;
begin
  Dialog := TCastleOpenDialog.Create(nil);
  try
    Dialog.Filter := LoadTiledMap_FileFilters;
    Dialog.AdviceDataDirectory := PropertyEditorsAdviceDataDirectory;
    if (GetStrValue = '') and PropertyEditorsAdviceDataDirectory then
      Dialog.URL := 'castle-data:/' // make it easier to open files within data folder
    else
      Dialog.URL := GetStrValue;
    if Dialog.Execute then
      SetStrValue(Dialog.URL);
  finally FreeAndNil(Dialog) end;
end;

{ TSoundUrlPropertyEditor ---------------------------------------------------- }

function TSoundUrlPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paMultiSelect];
end;

procedure TSoundUrlPropertyEditor.Edit;
var
  Dialog: TCastleOpenDialog;
begin
  Dialog := TCastleOpenDialog.Create(nil);
  try
    FileFiltersToDialog(LoadSound_FileFilters, Dialog);
    Dialog.AdviceDataDirectory := PropertyEditorsAdviceDataDirectory;
    if (GetStrValue = '') and PropertyEditorsAdviceDataDirectory then
      Dialog.URL := 'castle-data:/' // make it easier to open files within data folder
    else
      Dialog.URL := GetStrValue;
    if Dialog.Execute then
      SetStrValue(Dialog.URL);
  finally FreeAndNil(Dialog) end;
end;

{ TFontUrlPropertyEditor ---------------------------------------------------- }

function TFontURLPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paMultiSelect];
end;

procedure TFontURLPropertyEditor.Edit;
var
  Dialog: TCastleOpenDialog;
begin
  Dialog := TCastleOpenDialog.Create(nil);
  try
    FileFiltersToDialog(LoadFont_FileFilters, Dialog);
    Dialog.AdviceDataDirectory := PropertyEditorsAdviceDataDirectory;
    if (GetStrValue = '') and PropertyEditorsAdviceDataDirectory then
      Dialog.URL := 'castle-data:/' // make it easier to open files within data folder
    else
      Dialog.URL := GetStrValue;
    if Dialog.Execute then
      SetStrValue(Dialog.URL);
  finally FreeAndNil(Dialog) end;
end;

{$endif read_implementation}
