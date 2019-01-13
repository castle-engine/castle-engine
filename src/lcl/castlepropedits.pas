{
  Copyright 2010-2018 Michalis Kamburelis.

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

var
  PropertyEditorsAdviceDataDirectory: Boolean;

procedure Register;

implementation

uses SysUtils, Classes,
  PropEdits, ComponentEditors, LResources, Dialogs, Controls, LCLVersion,
  OpenGLContext, Graphics,
  CastleSceneCore, CastleScene, CastleLCLUtils, X3DLoad, X3DNodes,
  CastleUIControls, CastleControl, CastleControls, CastleImages, CastleTransform,
  CastleVectors, CastleUtils, CastleColors, CastleSceneManager, CastleDialogs,
  CastleTiledMap;

{ Define this for new Lazarus that has Options (with ocoRenderAtDesignTime)
  (see issue https://bugs.freepascal.org/view.php?id=32026 ). }
{$if LCL_FULLVERSION >= 1090000}
  {$define HAS_RENDER_AT_DESIGN_TIME}
{$endif}

{ TSubPropertiesEditor ----------------------------------------------------- }

type
  TSubPropertiesEditor = class(TClassPropertyEditor)
  public
    function  GetAttributes: TPropertyAttributes; Override;
  end;

function TSubPropertiesEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paReadOnly];
end;

{ TSceneURLPropertyEditor ---------------------------------------------------- }

type
  { Property editor for URL that refers to 3D model.
    Show an TCastleOpen3DDialog on Edit. }
  TSceneURLPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function TSceneURLPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

procedure TSceneURLPropertyEditor.Edit;
var
  Dialog: TCastleOpen3DDialog;
begin
  Dialog := TCastleOpen3DDialog.Create(nil);
  try
    Dialog.AdviceDataDirectory := PropertyEditorsAdviceDataDirectory;
    Dialog.URL := GetStrValue;
    if Dialog.Execute then
      SetStrValue(Dialog.URL);
  finally FreeAndNil(Dialog) end;
end;

{ TImageURLPropertyEditor ---------------------------------------------------- }

type
  { Property editor for URL that refers to a file readable by Castle Game Engine.
    Show an TCastleOpenImageDialog on Edit. }
  TImageURLPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function TImageURLPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

procedure TImageURLPropertyEditor.Edit;
var
  Dialog: TCastleOpenImageDialog;
begin
  Dialog := TCastleOpenImageDialog.Create(nil);
  try
    Dialog.AdviceDataDirectory := PropertyEditorsAdviceDataDirectory;
    Dialog.URL := GetStrValue;
    if Dialog.Execute then
      SetStrValue(Dialog.URL);
  finally FreeAndNil(Dialog) end;
end;

{ TDesignURLPropertyEditor ---------------------------------------------------- }

type
  { Property editor for URL that refers to a file readable by UserInterfaceLoad. }
  TDesignURLPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function TDesignURLPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

procedure TDesignURLPropertyEditor.Edit;
var
  Dialog: TCastleOpenDialog;
begin
  Dialog := TCastleOpenDialog.Create(nil);
  try
    Dialog.Filter := 'CGE User Interace Design (*.castle-user-interface)|*.castle-user-interface|All Files|*';
    Dialog.AdviceDataDirectory := PropertyEditorsAdviceDataDirectory;
    Dialog.URL := GetStrValue;
    if Dialog.Execute then
      SetStrValue(Dialog.URL);
  finally FreeAndNil(Dialog) end;
end;

{ TTiledMapURLPropertyEditor ---------------------------------------------------- }

type
  { Property editor for URL that refers to a Tiled Map file. }
  TTiledMapURLPropertyEditor = class(TStringPropertyEditor)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function TTiledMapURLPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paRevertable];
end;

procedure TTiledMapURLPropertyEditor.Edit;
var
  Dialog: TCastleOpenDialog;
begin
  Dialog := TCastleOpenDialog.Create(nil);
  try
    Dialog.Filter := 'Tiled Map (*.tmx)|*.tmx|All Files|*';
    Dialog.AdviceDataDirectory := PropertyEditorsAdviceDataDirectory;
    Dialog.URL := GetStrValue;
    if Dialog.Execute then
      SetStrValue(Dialog.URL);
  finally FreeAndNil(Dialog) end;
end;

{ TCastleColorPropertyEditor ------------------------------------------------- }

type
  TCastleColorPropertyEditor = class(TSubPropertiesEditor)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function TCastleColorPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paRevertable];
end;

procedure TCastleColorPropertyEditor.Edit;
var
  Dialog: TColorDialog;
  ColorPersistent: TCastleColorPersistent;
  Color: TCastleColor;
  ColorByte: TVector3Byte;
begin
  Dialog := TColorDialog.Create(nil);
  try
    ColorPersistent := (GetObjectValue as TCastleColorPersistent);
    Color := ColorPersistent.Value;
    ColorByte := Vector3Byte(Color.XYZ); // edit only Color RGB
    Dialog.Color := RGBToColor(ColorByte[0], ColorByte[1], ColorByte[2]);
    if Dialog.Execute then
    begin
      RedGreenBlue(Dialog.Color, ColorByte.Data[0], ColorByte.Data[1], ColorByte.Data[2]);
      // keep Color alpha unchanged ...
      Color := Vector4(Vector3(ColorByte), Color[3]);
      // ... unless it is zero, then change to 1.
      // This makes changing TCastleUserInterface.BorderColor changing more natural.
      if Color[3] = 0 then
        Color[3] := 1;
      ColorPersistent.Value := Color;
    end;
  finally FreeAndNil(Dialog) end;
end;

{ TCastleColorRGBPropertyEditor ------------------------------------------------- }

type
  TCastleColorRGBPropertyEditor = class(TSubPropertiesEditor)
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

function TCastleColorRGBPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes + [paDialog, paRevertable];
end;

procedure TCastleColorRGBPropertyEditor.Edit;
var
  Dialog: TColorDialog;
  ColorPersistent: TCastleColorRGBPersistent;
  Color: TCastleColorRGB;
  ColorByte: TVector3Byte;
begin
  Dialog := TColorDialog.Create(nil);
  try
    ColorPersistent := (GetObjectValue as TCastleColorRGBPersistent);
    Color := ColorPersistent.Value;
    ColorByte := Vector3Byte(Color);
    Dialog.Color := RGBToColor(ColorByte[0], ColorByte[1], ColorByte[2]);
    if Dialog.Execute then
    begin
      RedGreenBlue(Dialog.Color, ColorByte.Data[0], ColorByte.Data[1], ColorByte.Data[2]);
      Color := Vector3(ColorByte);
      ColorPersistent.Value := Color;
    end;
  finally FreeAndNil(Dialog) end;
end;

{ TChildrenControlsPropertyEditor -------------------------------------------- }

type
  TChildrenControlsPropertyEditor = class(TListPropertyEditor)
  end;

(*

// This was just a temporary thing until we can use CGE editor.

{ T3DEditorForm -------------------------------------------------------------- }

type
  T3DEditorForm = class(TComponent)
    Control: TCastleControl;
    SceneManager: TCastleSceneManager; //< just a shortcut for TCastleControl.SceneManager now
    Items: TSceneManagerWorld; //< just a shortcut for SceneManager.Items now
    procedure ShowModal;
  end;

procedure T3DEditorForm.ShowModal;
var
  Transform: TCastleTransform;
  Scene: TCastleScene;
  Root: TX3DRootNode;
  Shape: TShapeNode;
begin
  if MessageDlg('Edit Game World',
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
    Root.AddChildren(Shape);

    Scene := TCastleScene.Create(Items);
    Scene.Load(Root, true);

    Transform := TCastleTransform.Create(Items);
    Transform.Translation := Vector3(
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

    {$ifdef HAS_RENDER_AT_DESIGN_TIME}
    Control.Options := Control.Options + [ocoRenderAtDesignTime];
    {$endif}
  end;
end;

{ TSceneManagerWorldPropertyEditor ----------------------------------------------------- }

type
  TSceneManagerWorldPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; Override;
    function  GetAttributes: TPropertyAttributes; Override;
  end;

procedure TSceneManagerWorldPropertyEditor.Edit;
var
  Dialog: T3DEditorForm;
begin
  Dialog := T3DEditorForm.Create(nil);
  try
    Dialog.Items := TSceneManagerWorld(GetObjectValue(TSceneManagerWorld));
    Dialog.ShowModal;
  finally FreeAndNil(Dialog) end;
end;

function TSceneManagerWorldPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, {paDialog,} paReadOnly];
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
    Dialog.Control := Control;
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

*)

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleSceneCore,
    'URL', TSceneURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleImageControl,
    'URL', TImageURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleDesign,
    'URL', TDesignURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleTiledMapControl,
    'URL', TTiledMapURLPropertyEditor);

  // TODO: the SceneManager.Items, actually complete TCastleControl,
  // should be editable using new CGE editor now.

  { TODO: crashes
  RegisterPropertyEditor(TypeInfo(TChildrenControls), TCastleControlCustom,
    'Controls', TChildrenControlsPropertyEditor);
  }
  // RegisterComponentEditor(TCastleControl, TCastleControlComponentEditor);

  { These descend from TSubPropertiesEditor,
    which is necessary to expand in castle-editor and Lazarus design-time.
    Although it seems not necessary for object inspector in castle-editor? }
  RegisterPropertyEditor(TypeInfo(TSceneManagerWorld), TCastleSceneManager, 'Items',
    TSubPropertiesEditor);
  RegisterPropertyEditor(TypeInfo(TCastleColorPersistent), nil, '',
    TCastleColorPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleColorRGBPersistent), nil, '',
    TCastleColorRGBPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector3Persistent), nil, '',
    TSubPropertiesEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector4Persistent), nil, '',
    TSubPropertiesEditor);
  RegisterPropertyEditor(TypeInfo(TBorder), nil, '',
    TSubPropertiesEditor);
end;

initialization
  { Add lrs with icons, following
    http://wiki.lazarus.freepascal.org/Lazarus_Packages#Add_a_component_icon }
  {$I icons/castleicons.lrs}
end.
