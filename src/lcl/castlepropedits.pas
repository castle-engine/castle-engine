{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Property and component editors using LCL.
  These can be used when we're inside Lazarus IDE or inside CGE editor.

  For documentation how to create property editors, component editors etc. see
  - http://wiki.freepascal.org/How_To_Write_Lazarus_Component#Component_editors
  - comments of Lazarus ideintf/propedits.pp sources. }
unit CastlePropEdits;

{$I castleconf.inc}

interface

uses PropEdits;

{ Some of the property editors are publicly exposed,
  to enable using them for your own properties in custom components too.
  See examples/advanced_editor/custom_component/code/gamecontrols.pas . }
{$define read_interface}
{$I castlepropedits_any_subproperties.inc}
{$I castlepropedits_url.inc}
{$undef read_interface}

procedure Register;

implementation

uses // FPC and LCL units
  SysUtils, Classes, TypInfo, Forms, Math,
  LResources, Dialogs, Controls, LCLVersion, OpenGLContext, Graphics, ObjInspStrConsts,
  // Lazarus design-time (IDE) units
  ComponentEditors,
  // CGE units
  CastleSceneCore, CastleScene, CastleLCLUtils, X3DLoad, X3DNodes, CastleCameras,
  CastleUIControls, CastleControl, CastleControls, CastleImages, CastleTransform,
  CastleVectors, CastleRectangles, CastleUtils, CastleColors, CastleViewport,
  CastleDialogs, CastleComponentSerialize,
  CastleTiledMap, CastleGLImages, CastleStringUtils, CastleFilesUtils,
  CastleInternalExposeTransformsDialog, CastleInternalTiledLayersDialog,
  CastleInternalRegionDialog,
  CastleSoundEngine, CastleFonts,
  CastleScriptParser, CastleInternalLclDesign, CastleTerrain, CastleLog,
  CastleEditorAccess, CastleRenderOptions, CastleThirdPersonNavigation;

{ Wrap given floating-point expression in CastleScript in "deg(...)".
  If it's already wrapped, leave it as-is.}
function WrapDegIfNeeded(const FloatExpression: String): String;
begin
  if IsPrefix('deg', Trim(FloatExpression), true) then
    { If user explicitly left "deg(" marker, that's OK, leave it. }
    Result := FloatExpression
  else
    { If user deleted "deg(" marker, interpret result as degrees
      *anyway*. This is more user-friendly, Blender and Godot also do
      it like this. }
    Result := 'deg(' + FloatExpression + ')';
end;

{$define read_implementation}
{$I castlepropedits_url.inc}

{ These include files are only used in the implementation,
  they don't look at read_interface/read_implementation symbols. }
{$I castlepropedits_any_subproperties.inc}
{$I castlepropedits_autoanimation.inc}
{$I castlepropedits_meshcolliderscene.inc}
{$I castlepropedits_vector.inc}
{$I castlepropedits_image.inc}
{$I castlepropedits_region.inc}
{$I castlepropedits_number.inc}
{$I castlepropedits_exposetransforms.inc}
{$I castlepropedits_tiledlayers.inc}
{$I castlepropedits_rangeset.inc}
{$I castlepropedits_3dcoords.inc}
{$I castlepropedits_colorchannels.inc}
{$I castlepropedits_component_transform.inc}
{$I castlepropedits_component_scene.inc}
{$I castlepropedits_component_transformreference.inc}
{$I castlepropedits_component_imagetransform.inc}
{$I castlepropedits_component_imagecontrol.inc}
{$I castlepropedits_component_transformdesign.inc}
{$I castlepropedits_component_design.inc}
{$I castlepropedits_component_joints.inc}
{$I castlepropedits_abstracttwobodiesjoint.inc}

procedure Register;
begin
  { URL properties }
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleSceneCore,
    'URL', TSceneUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleImageControl,
    'URL', TImageUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleImagePersistent,
    'URL', TImageUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleAbstractPrimitive,
    'Texture', TImageUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleAbstractPrimitive,
    'TextureNormalMap', TImageUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleBitmapFont,
    'ImageUrl', TImageUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleImageTransform,
    'Url', TImageUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleImageTransform,
    'UrlNormalMap', TImageUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleTerrainImage,
    'Url', TImageUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleTerrainLayer,
    'Texture', TImageUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleDesign,
    'URL', TUiDesignUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleTransformDesign,
    'URL', TTransformDesignUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleComponentFactory,
    'Url', TAnyDesignUrlPropertyEditor);
  {$warnings off} // define to support deprecated, for now
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleTiledMapControl,
    'URL', TTiledMapUrlPropertyEditor);
  {$warnings on}
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleTiledMap,
    'URL', TTiledMapUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleSound,
    'URL', TSoundUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleFont,
    'URL', TFontUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleBackground,
    'TextureNegativeX', TImageUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleBackground,
    'TextureNegativeY', TImageUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleBackground,
    'TextureNegativeZ', TImageUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleBackground,
    'TexturePositiveX', TImageUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleBackground,
    'TexturePositiveY', TImageUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleBackground,
    'TexturePositiveZ', TImageUrlPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleControl,
    'DesignUrl', TUiDesignUrlPropertyEditor);

  { Improved numeric properties }
  RegisterPropertyEditor(TypeInfo(Single), nil, '', TCastleFloatPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Double), nil, '', TCastleFloatPropertyEditor);
  {$ifndef EXTENDED_EQUALS_DOUBLE}
  RegisterPropertyEditor(TypeInfo(Extended), nil, '', TCastleFloatPropertyEditor);
  {$endif}
  {$ifdef CPU64}
  RegisterPropertyEditor(TypeInfo(PtrInt), TComponent, 'Tag', TCastleTagPropertyEditor);
  {$endif}
  RegisterPropertyEditor(TypeInfo(Single), TCastleVector4RotationPersistent, 'W',
    TCastleFloatRotationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Single), TCastleImagePersistent, 'Rotation',
    TCastleFloatRotationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Single), TCastleImageControl, 'Rotation',
    TCastleFloatRotationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Single), TCastleSpotLight, 'BeamWidth',
    TCastleFloatRotationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Single), TCastleSpotLight, 'CutOffAngle',
    TCastleFloatRotationPropertyEditor);

  { Handle using TCastleRegionEditor.
    Note: TBorder rule with 'ProtectedSides' name is registered
    before registering for TBorder with any name below.
    (Not tested if it's really necessary, but seems safer). }
  RegisterPropertyEditor(TypeInfo(TBorder), nil, 'ProtectedSides',
    TCastleRegionEditor);
  RegisterPropertyEditor(TypeInfo(TFloatRectanglePersistent), nil, 'RegionPersistent',
    TCastleRegionEditor);

  { Properties that simply use TSubPropertiesEditor.
    Registering properties that use TSubPropertiesEditor
    (not any descendant of it) is still necessary to expand them
    in castle-editor and Lazarus design-time and allow to edit multiple values
    when multiple components are selected. }
  RegisterPropertyEditor(TypeInfo(TCastleRootTransform), TCastleViewport, 'Items',
    TSubPropertiesEditor);
  RegisterPropertyEditor(TypeInfo(TBorder), nil, '',
    TSubPropertiesEditor);
  RegisterPropertyEditor(TypeInfo(TCastleRenderOptions), nil, '',
    TSubPropertiesEditor);

  { Other properties }
  RegisterPropertyEditor(TypeInfo(TCastleImagePersistent), nil, '',
    TCastleImagePersistentEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector2Persistent), nil, '',
    TCastleVector2PropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector3Persistent), TCastleTransform, 'ScalePersistent',
    TScalePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector3Persistent), TCastleBox, 'SizePersistent',
    TScalePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector3Persistent), TCastleBoxCollider, 'SizePersistent',
    TScalePropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector2Persistent), TCastlePlane, 'SizePersistent',
    TSize2DPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector2Persistent), TCastleImageTransform, 'SizePersistent',
    TSize2DPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector2Persistent), TCastleAbstractPrimitive, 'TextureScalePersistent',
    TSize2DPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector3Persistent), nil, '',
    TCastleVector3PropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector4Persistent), nil, '',
    TCastleVector4PropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleVector4RotationPersistent), nil, '',
    TCastleVectorRotationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleSceneCore, 'AutoAnimation',
    TSceneAutoAnimationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TCastleSceneCore, 'ExposeTransforms',
    TExposeTransformsPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TCastleTiledMap.TLayers), TCastleTiledMap, 'Layers',
    TTiledLayersPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TCastleTransform), TCastleMeshCollider, 'Mesh',
    TMeshColliderMeshPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TCastleTransform), TCastleAbstractTwoBodiesJoint, 'Connected',
    TConnectedPropertyEditor);

  { sets }
  RegisterPropertyEditor(TypeInfo(T3DCoords), nil, '',
    T3DCoordsRangeSetPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TColorChannels), nil, '',
    TColorChannelsRangeSetPropertyEditor);

  { animations on TCastleThirdPersonNavigation }
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleThirdPersonNavigation, 'AnimationIdle',
    TThirdPersonAnimationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleThirdPersonNavigation, 'AnimationWalk',
    TThirdPersonAnimationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleThirdPersonNavigation, 'AnimationRun',
    TThirdPersonAnimationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleThirdPersonNavigation, 'AnimationJump',
    TThirdPersonAnimationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleThirdPersonNavigation, 'AnimationRotate',
    TThirdPersonAnimationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleThirdPersonNavigation, 'AnimationCrouch',
    TThirdPersonAnimationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleThirdPersonNavigation, 'AnimationCrouchIdle',
    TThirdPersonAnimationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleThirdPersonNavigation, 'AnimationCrouchRotate',
    TThirdPersonAnimationPropertyEditor);
  RegisterPropertyEditor(TypeInfo(AnsiString), TCastleThirdPersonNavigation, 'AnimationFall',
    TThirdPersonAnimationPropertyEditor);

  // various RegisterComponentEditor
  RegisterComponentEditor(TCastleTransform, TCastleTransformComponentEditor);
  RegisterComponentEditor(TCastleTransformReference, TCastleTransformReferenceComponentEditor);
  RegisterComponentEditor(TCastleScene, TCastleSceneComponentEditor);
  RegisterComponentEditor(TCastleImageTransform, TCastleImageTransformComponentEditor);
  RegisterComponentEditor(TCastleImageControl, TCastleImageControlComponentEditor);
  RegisterComponentEditor(TCastleTransformDesign, TCastleTransformDesignComponentEditor);
  RegisterComponentEditor(TCastleDesign, TCastleDesignComponentEditor);

  // RegisterComponentEditor on joints
  RegisterComponentEditor(TCastleHingeJoint, TCastleJointsComponentEditor);
  RegisterComponentEditor(TCastleRopeJoint, TCastleJointsComponentEditor);
  RegisterComponentEditor(TCastleDistanceJoint, TCastleJointsComponentEditor);
  RegisterComponentEditor(TCastleBallJoint, TCastleJointsComponentEditor);
  RegisterComponentEditor(TCastleGrabJoint, TCastleJointsComponentEditor);
  {$ifdef CASTLE_EXPERIMENTAL_JOINTS}
  RegisterComponentEditor(TCastleFixedJoint, TCastleJointsComponentEditor);
  RegisterComponentEditor(TCastleWorldPlaneDistanceJoint, TCastleJointsComponentEditor);
  RegisterComponentEditor(TCastlePulleyJoint, TCastleJointsComponentEditor);
  RegisterComponentEditor(TCastleSliderJoint, TCastleJointsComponentEditor);
  {$endif CASTLE_EXPERIMENTAL_JOINTS}
end;

initialization
  { Add lrs with icons, following
    http://wiki.lazarus.freepascal.org/Lazarus_Packages#Add_a_component_icon }
  {$I icons/castleicons.lrs}
end.
