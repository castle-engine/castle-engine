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

{ Lazarus IDE integration with Castle Game Engine:
  - property editors,
  - component editors,
  - making castle-data: work in Lazarus IDE,
  - sending CGE warnings to Lazarus IDE messages.

  Property and component editors are also used when inside CGE editor.

  For documentation how to create property editors, component editors etc. see
  - http://wiki.freepascal.org/How_To_Write_Lazarus_Component#Component_editors
  - comments of Lazarus ideintf/propedits.pp sources. }
unit CastlePropEdits;

{$I castleconf.inc}

interface

procedure Register;

{ At design-time (when running in Lazarus IDE) make sure to adjust
  ApplicationDataOverride. }
procedure FixApplicationDataInIDE;

implementation

uses // FPC and LCL units
  SysUtils, Classes, TypInfo, Forms,
  LResources, Dialogs, Controls, LCLVersion, OpenGLContext, Graphics,
  // Lazarus design-time (IDE) units
  PropEdits, ComponentEditors, LazIDEIntf, IDEMsgIntf, IDEExternToolIntf,
  // CGE units
  CastleSceneCore, CastleScene, CastleLCLUtils, X3DLoad, X3DNodes, CastleCameras,
  CastleUIControls, CastleControl, CastleControls, CastleImages, CastleTransform,
  CastleVectors, CastleUtils, CastleColors, CastleViewport, CastleDialogs,
  CastleTiledMap, CastleGLImages, CastleStringUtils, CastleFilesUtils,
  CastleInternalExposeTransformsDialog, CastleSoundEngine, CastleFonts,
  CastleScriptParser, CastleApplicationProperties;

{$I castlepropedits_url.inc}
{$I castlepropedits_any_subproperties.inc}
{$I castlepropedits_autoanimation.inc}
{$I castlepropedits_color.inc}
{$I castlepropedits_vector.inc}
{$I castlepropedits_image.inc}
{$I castlepropedits_number.inc}
{$I castlepropedits_exposetransforms.inc}

procedure FixApplicationDataInIDE;
begin
  { TODO: This should be somehow registered (in Register) to listen to IDE project change,
    and adjust ApplicationDataOverride at project change.

    Doing it now, when DesignUrl seems needed (
    by dialog box that sets DesignUrl,
    by setting TCastleControl.DesignUrl in deserialization)
    is not a clean way to do this (because we may miss some situations). }

  if CastleDesignMode and
     (LazarusIDE <> nil) and
     (LazarusIDE.ActiveProject <> nil) then
  begin
    { Override ApplicationData interpretation, and castle-data:/xxx URL meaning. }
    ApplicationDataOverride := FilenameToURISafeUTF8(
      InclPathDelim(LazarusIDE.ActiveProject.Directory) + 'data' + PathDelim);
  end;
end;

type
  { Provides integration between Lazarus IDE and CGE that is nice to wrap
    in a class. }
  TCastleLazarusIDEIntegration = class(TComponent)
  strict private
    procedure WarningToLazarusMessages(const Category, S: string);
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TCastleLazarusIDEIntegration.Create(AOwner: TComponent);
begin
  inherited;
  ApplicationProperties.OnWarning.Add({$ifdef FPC}@{$endif}WarningToLazarusMessages);
end;

procedure TCastleLazarusIDEIntegration.WarningToLazarusMessages(const Category, S: string);
var
  MessageContent: String;
begin
  MessageContent := 'Castle Game Engine: ';
  if Category <> '' then
    MessageContent += Category + ': ';
  MessageContent += S;
  AddIDEMessage(mluWarning, MessageContent);
end;

procedure Register;
//var
//  LazarusIDEIntegration: TCastleLazarusIDEIntegration;
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

  { Improved numeric properties }
  RegisterPropertyEditor(TypeInfo(Single), nil, '', TCastleFloatPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Double), nil, '', TCastleFloatPropertyEditor);
  {$ifndef EXTENDED_EQUALS_DOUBLE}
  RegisterPropertyEditor(TypeInfo(Extended), nil, '', TCastleFloatPropertyEditor);
  {$endif}
  {$ifdef CPU64}
  RegisterPropertyEditor(TypeInfo(PtrInt), TComponent, 'Tag', TCastleTagPropertyEditor);
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
  RegisterPropertyEditor(TypeInfo(TCastleVector3Persistent), TCastleBox, 'SizePersistent',
    TScalePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector2Persistent), TCastlePlane, 'SizePersistent',
    TSize2DPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector3Persistent), nil, '',
    TCastleVector3PropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector4Persistent), nil, '',
    TCastleVector4PropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleSceneCore, 'AutoAnimation',
    TSceneAutoAnimationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TCastleSceneCore, 'ExposeTransforms',
    TExposeTransformsPropertyEditor);

  { If running in Lazarus IDE, show CGE warnings on Lazarus messages list. }
  {LazarusIDEIntegration := }TCastleLazarusIDEIntegration.Create(Application);
end;

initialization
  { Add lrs with icons, following
    http://wiki.lazarus.freepascal.org/Lazarus_Packages#Add_a_component_icon }
  {$I icons/castleicons.lrs}
end.
