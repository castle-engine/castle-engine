{%MainUnit castlepropedits.pas}
{
  Copyright 2010-2022 Michalis Kamburelis.

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
    Show an TCastleOpen3DDialog on Edit. }
  TSceneURLPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { Property editor for URL that refers to an image readable by Castle Game Engine.
    Show an TCastleOpenImageDialog on Edit. }
  TImageURLPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { Property editor for URL that refers to a file readable by UserInterfaceLoad. }
  TDesignURLPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { Property editor for URL that refers to a file readable by TransformLoad. }
  TTransformDesignURLPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { Property editor for URL that refers to a Tiled Map file. }
  TTiledMapURLPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { Property editor for URL that refers to a sound file. }
  TSoundURLPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { Property editor for URL that refers to a font file. }
  TFontURLPropertyEditor = class(TStringPropertyEditor)
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

{ TSceneURLPropertyEditor ---------------------------------------------------- }

function TSceneURLPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paMultiSelect];
end;

procedure TSceneURLPropertyEditor.Edit;
var
  Dialog: TCastleOpen3DDialog;
begin
  Dialog := TCastleOpen3DDialog.Create(nil);
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

{ TImageURLPropertyEditor ---------------------------------------------------- }

function TImageURLPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paMultiSelect];
end;

procedure TImageURLPropertyEditor.Edit;
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

{ TDesignURLPropertyEditor ---------------------------------------------------- }

function TDesignURLPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paMultiSelect];
end;

procedure TDesignURLPropertyEditor.Edit;
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

{ TTransformDesignURLPropertyEditor ---------------------------------------------------- }

function TTransformDesignURLPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paMultiSelect];
end;

procedure TTransformDesignURLPropertyEditor.Edit;
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

{ TTiledMapURLPropertyEditor ---------------------------------------------------- }

function TTiledMapURLPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paMultiSelect];
end;

procedure TTiledMapURLPropertyEditor.Edit;
var
  Dialog: TCastleOpenDialog;
begin
  Dialog := TCastleOpenDialog.Create(nil);
  try
    Dialog.Filter := 'Tiled Map (*.tmx)|*.tmx|All Files|*';
    Dialog.AdviceDataDirectory := PropertyEditorsAdviceDataDirectory;
    if (GetStrValue = '') and PropertyEditorsAdviceDataDirectory then
      Dialog.URL := 'castle-data:/' // make it easier to open files within data folder
    else
      Dialog.URL := GetStrValue;
    if Dialog.Execute then
      SetStrValue(Dialog.URL);
  finally FreeAndNil(Dialog) end;
end;

{ TSoundURLPropertyEditor ---------------------------------------------------- }

function TSoundURLPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable, paMultiSelect];
end;

procedure TSoundURLPropertyEditor.Edit;
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

{ TFontURLPropertyEditor ---------------------------------------------------- }

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
