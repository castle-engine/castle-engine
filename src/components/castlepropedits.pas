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

{ Property and component editors for Lazarus components.
  For docs, see
  - http://wiki.freepascal.org/How_To_Write_Lazarus_Component#Component_editors
  - comments of Lazarus ideintf/propedits.pp sources.

  @exclude (This unit is not supposed to be normally used, so not documented
  by PasDoc. It's only for Lazarus registration.) }
unit CastlePropEdits;

{$I castleconf.inc}

interface

procedure Register;

implementation

uses SysUtils, Classes,
  PropEdits, ComponentEditors, LResources, Dialogs, Controls,
  CastleSceneCore, CastleScene, CastleLCLUtils, X3DLoad, X3DNodes,
  CastleUIControls, CastleControl, CastleControls, CastleImages, Castle3D,
  CastleVectors, CastleUtils, CastleColors, CastleSceneManager;

{ TSceneURLPropertyEditor ---------------------------------------------------- }

type
  TSceneURLPropertyEditor = class(TFileNamePropertyEditor)
  public
    function GetFilter: String; override;
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

{ TImageURLPropertyEditor ---------------------------------------------------- }

type
  TImageURLPropertyEditor = class(TFileNamePropertyEditor)
  public
    function GetFilter: String; override;
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

{ TChildrenControlsPropertyEditor -------------------------------------------- }

type
  TChildrenControlsPropertyEditor = class(TListPropertyEditor)
  end;

{ T3DEditorForm -------------------------------------------------------------- }

type
  T3DEditorForm = class(TComponent)
    Items: T3DWorld;
    SceneManager: TCastleSceneManager;
    procedure ShowModal;
  end;

procedure T3DEditorForm.ShowModal;
var
  Transform: T3DTransform;
  Scene: TCastleScene;
  Root: TX3DRootNode;
  Shape: TShapeNode;
begin
  if MessageDlg('Edit game world',
    'TODO: Here we will implement a real editor to edit SceneManager.Items.' + NL +
    NL +
    'For now: do you want to add a yellow 3D sphere to your world (with a little randomized location)?',
    mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    Shape := TShapeNode.Create;
    Shape.Geometry := TSphereNode.Create;
    Shape.Material := TMaterialNode.Create;
    Shape.Material.DiffuseColor := YellowRGB;

    Root := TX3DRootNode.Create;
    Root.FdChildren.Add(Shape);

    Scene := TCastleScene.Create(Items);
    Scene.Load(Root, true);

    Transform := T3DTransform.Create(Items);
    Transform.Translation := Vector3Single(
      RandomFloatRange(-1, 1),
      RandomFloatRange(-1, 1),
      RandomFloatRange(-1, 1)
    );
    Transform.Add(Scene);

    Items.Add(Transform);

    if SceneManager <> nil then
    begin
      // set MainScene, to set headlight
      if SceneManager.MainScene = nil then
        SceneManager.MainScene := Scene;
      // force recreating the camera soon, to see the whole scene
      SceneManager.Camera.Free;
    end;
  end;
end;

{ T3DWorldPropertyEditor ----------------------------------------------------- }

type
  T3DWorldPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; Override;
    function  GetAttributes: TPropertyAttributes; Override;
  end;

procedure T3DWorldPropertyEditor.Edit;
var
  Dialog: T3DEditorForm;
begin
  Dialog := T3DEditorForm.Create(nil);
  try
    Dialog.Items := T3DWorld(GetObjectValue(T3DWorld));
    Dialog.ShowModal;
  finally FreeAndNil(Dialog) end;
end;

function T3DWorldPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paDialog, paReadOnly];
end;

{ TCastleControlComponentEditor ---------------------------------------------- }

type
  TCastleControlComponentEditor = class(TComponentEditor)
  protected
    procedure DoShowEditor;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

procedure TCastleControlComponentEditor.DoShowEditor;
var
  Dialog: T3DEditorForm;
  Control: TCastleControl;
  SceneManager: TCastleSceneManager;
begin
  Dialog := T3DEditorForm.Create(nil);
  try
    Control := GetComponent as TCastleControl;
    SceneManager := Control.SceneManager;
    Dialog.Items := SceneManager.Items;
    Dialog.SceneManager := SceneManager;
    Dialog.ShowModal;
    Control.Invalidate;
  finally FreeAndNil(Dialog) end;
end;

procedure TCastleControlComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: DoShowEditor;
  end;
end;

function TCastleControlComponentEditor.GetVerb(Index: Integer): String;
begin
  Result := 'Edit the Game World (SceneManager.Items)...';
end;

function TCastleControlComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
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
  // TODO: Also TSceneManagerPropertyEditor, leading to the same?
  RegisterPropertyEditor(TypeInfo(T3DWorld), TCastleSceneManager, 'Items',
    T3DWorldPropertyEditor);
  RegisterComponentEditor(TCastleControl, TCastleControlComponentEditor);
end;

initialization
  { Add lrs with icons, following
    http://wiki.lazarus.freepascal.org/Lazarus_Packages#Add_a_component_icon }
  {$I icons/castleicons.lrs}
end.
