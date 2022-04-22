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

{ Property and component editors for components.
  These are used by object inspectors (inside Lazarus or CGE editor).

  For documentation how to create property editors, component editors etc. see
  - http://wiki.freepascal.org/How_To_Write_Lazarus_Component#Component_editors
  - comments of Lazarus ideintf/propedits.pp sources.

  @exclude This unit is not supposed to be used by normal developers.
  It should only be used to register the editors (in Lazarus, in CGE editor). }
unit CastlePropEdits;

{$I castleconf.inc}

interface

uses PropEdits;

{$define read_interface}
{$I castlepropedits_url.inc}
{$undef read_interface}

procedure Register;

implementation

uses SysUtils, Classes, TypInfo, Forms,
  ComponentEditors, LResources, Dialogs, Controls, LCLVersion, LazIDEIntf,
  OpenGLContext, Graphics,
  CastleSceneCore, CastleScene, CastleLCLUtils, X3DLoad, X3DNodes, CastleCameras,
  CastleUIControls, CastleControl, CastleControls, CastleImages, CastleTransform,
  CastleVectors, CastleUtils, CastleColors, CastleViewport, CastleDialogs,
  CastleTiledMap, CastleGLImages, CastleStringUtils, CastleFilesUtils,
  CastleInternalExposeTransformsDialog, CastleSoundEngine, CastleFonts,
  CastleScriptParser;

function PropertyEditorsAdviceDataDirectory: Boolean;
begin
  { There's no reason to leave it false, in practice. }
  Result := true;

  if CastleDesignMode and
     (LazarusIDE <> nil) and
     (LazarusIDE.ActiveProject <> nil) then
  begin
    { Override ApplicationData interpretation, and castle-data:/xxx URL meaning.
      This allows PropertyEditorsAdviceDataDirectory to work
      inside Lazarus IDE, e.g. when setting TCastleControl.DesignUrl. }
    ApplicationDataOverride := FilenameToURISafeUTF8(
      InclPathDelim(LazarusIDE.ActiveProject.Directory) + 'data' + PathDelim);
  end;
end;

{$define read_implementation}
{$I castlepropedits_url.inc}

{ These include files are only used in the implementation,
  they don't look at read_interface/read_implementation symbols. }
{$I castlepropedits_any_subproperties.inc}
{$I castlepropedits_autoanimation.inc}
{$I castlepropedits_color.inc}
{$I castlepropedits_vector.inc}
{$I castlepropedits_image.inc}
{$I castlepropedits_float.inc}
{$I castlepropedits_exposetransforms.inc}

procedure Register;
begin
  { URL properties }
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleSceneCore,
    'URL', TSceneURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleImageControl,
    'URL', TImageURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleImagePersistent,
    'URL', TImageURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleAbstractPrimitive,
    'Texture', TImageURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleAbstractPrimitive,
    'TextureNormalMap', TImageURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleBitmapFont,
    'ImageUrl', TImageURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleImageTransform,
    'Url', TImageURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleDesign,
    'URL', TDesignURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleTransformDesign,
    'URL', TTransformDesignURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleTiledMapControl,
    'URL', TTiledMapURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleSound,
    'URL', TSoundURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleFont,
    'URL', TFontURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleBackground,
    'TextureNegativeX', TImageURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleBackground,
    'TextureNegativeY', TImageURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleBackground,
    'TextureNegativeZ', TImageURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleBackground,
    'TexturePositiveX', TImageURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleBackground,
    'TexturePositiveY', TImageURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleBackground,
    'TexturePositiveZ', TImageURLPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleControl,
    'DesignUrl', TDesignURLPropertyEditor);

  { Improved float properties }
  RegisterPropertyEditor(TypeInfo(Single), nil, '', TCastleFloatPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Double), nil, '', TCastleFloatPropertyEditor);
  {$ifndef EXTENDED_EQUALS_DOUBLE}
  RegisterPropertyEditor(TypeInfo(Extended), nil, '', TCastleFloatPropertyEditor);
  {$endif}

  { Properties that simply use TSubPropertiesEditor.
    Registering properties that use TSubPropertiesEditor
    (not any descendant of it) is still necessary to expand them
    in castle-editor and Lazarus design-time. }
  RegisterPropertyEditor(TypeInfo(TCastleRootTransform), TCastleViewport, 'Items',
    TSubPropertiesEditor);
  RegisterPropertyEditor(TypeInfo(TBorder), nil, '',
    TSubPropertiesEditor);

  { Other properties }
  RegisterPropertyEditor(TypeInfo(TCastleImagePersistent), nil, '',
    TCastleImagePersistentEditor);
  RegisterPropertyEditor(TypeInfo(TCastleColorPersistent), nil, '',
    TCastleColorPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleColorRGBPersistent), nil, '',
    TCastleColorRGBPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector2Persistent), nil, '',
    TCastleVector2PropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector3Persistent), TCastleTransform, 'ScalePersistent',
    TScalePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector3Persistent), nil, '',
    TCastleVector3PropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector4Persistent), nil, '',
    TCastleVector4PropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleSceneCore, 'AutoAnimation',
    TSceneAutoAnimationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TCastleSceneCore, 'ExposeTransforms',
    TExposeTransformsPropertyEditor);
end;

initialization
  { Add lrs with icons, following
    http://wiki.lazarus.freepascal.org/Lazarus_Packages#Add_a_component_icon }
  {$I icons/castleicons.lrs}
end.
