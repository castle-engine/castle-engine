{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { For PasDoc: below is a trick to convince PasDoc that EX3DError is a class.

    Otherwise, PasDoc doesn't understand it, and places EX3DError in
    the "Class Hierarchy" (since it's an ancestor of some other classes....)
    but in the incorrect place (not descending from Exception, despite
    external_class_hierarchy.txt.)
    That's because original EX3DError is in unparsed by PasDoc (internal)
    CastleInternalX3DLexer unit. }
  { Any error related to VRML/X3D. }
  {$ifdef PASDOC}
  EX3DError = class(Exception);
  {$else}
  EX3DError = CastleInternalX3DLexer.EX3DError;
  {$endif}

  EX3DFieldAssign = class(EX3DError);
  EX3DFieldAssignInvalidClass = class(EX3DFieldAssign);
  { Raised by various X3D methods searching for X3D items (nodes, fields,
    events and such) when given item cannot be found. }
  EX3DNotFound = class(EX3DError);

  EInvalidNurbs = class(EX3DError);

  TSaveToXmlMethod = (sxNone, sxAttribute, sxAttributeCustomQuotes, sxChildElement);

  { Possible things that happen when given field is changed.
    Used by TX3DField.ExecuteChanges. }
  TX3DChange = (
    { Something visible in the geometry changed.
      See vcVisibleGeometry.
      This means that VisibleChangeHere with vcVisibleGeometry included should
      be called. }
    chVisibleGeometry,

    { Something visible changed, but not geometry.
      See vcVisibleNonGeometry.
      This means that VisibleChangeHere with vcVisibleNonGeometry included should
      be called. }
    chVisibleNonGeometry,

    { Call VisibleChangeHere to redisplay the scene.

      If you include one of the chVisibleGeometry or chVisibleNonGeometry
      then this flag (chRedisplay) makes no effect.
      Otherwise, this flag should be used if your change requires
      redisplay of the 3D view for some other reason. }
    chRedisplay,

    { Transformation of children of this node changed.

      Caller will analyze the scene (your children) to know what this implicates,
      don't include other flags with this. }
    chTransform,

    { Coordinate (both VRML 1.0 and >= 2.0) node "point" field changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chCoordinate,

    { Something visible in VRML 1.0 state node (that may be present
      in TX3DGraphTraverseState.VRML1State) changed, but not geometry.
      Excluding Coordinate node change (this one should go through chCoordinate
      only).

      This is allowed, and ignored, on nodes that are not part of VRML 1.0
      state. (This is useful for alphaChannel field, that is declared
      in TAbstractGeometryNode, and so is part of some VRML 1.0 state nodes
      but is also part of VRML >= 2.0 nodes.)

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this.
      Exception: you can (and should) include chUseBlending and
      chTextureImage for appropriate changes. }
    chVisibleVRML1State,

    { Some visible geometry changed because of VRML 1.0 state node change.
      This is for VRML 1.0 state node changes, excluding non-geometry changes
      (these go to chVisibleVRML1State) and Coordinate changes (these go to
      chCoordinate).

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chGeometryVRML1State,

    { Something visible in VRML >= 2.0 Material (or TwoSidedMaterial) changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this.
      Exception: you can (and should) include chUseBlending for appropriate
      Material changes. }
    chMaterial2,

    { Something that may affect UseBlending calculation possibly changed.
      This is guaranteed to work only when used together with
      chVisibleVRML1State and chMaterial2. It's understood that only
      shapes that use given material need UseBlending recalculated. }
    chUseBlending,

    { Light property that is also reflected in TLightInstance structure.
      Only allowed on node's descending from TAbstractLightNode.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this.
      Exception: include also chLightLocationDirection when appropriate. }
    chLightInstanceProperty,

    { Light's location and/or direction changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this.
      Exception: include also chLightInstanceProperty when appropriate. }
    chLightLocationDirection,

    { TCastleSceneCore.MainLightForShadows possibly changed because of this change.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chLightForShadowVolumes,

    { Switch.whichChoice changed, for VRML >= 2.0.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chSwitch2,

    { X3DColorNode colors changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chColorNode,

    { X3DTextureCoordinateNode coords changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chTextureCoordinate,

    { VRML >= 2.0 TextureTransform changed.
      Not for multi-texture node changes, only the simple nodes changes.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chTextureTransform,

    { Geometry node visible (or collidable) changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chGeometry,

    { X3DEnvironmentalSensorNode bounds (size/center) changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chEnvironmentalSensorBounds,

    { TimeDependent node is start/stop/pause/resume time changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chTimeStopStart,

    { Viewpoint vectors (position, direction, up, gravity up) changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chViewpointVectors,

    { Viewpoint projection changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chViewpointProjection,

    { Texture image (data) needs reloading (url or source SFImage
      data changed). This is for TAbstractTexture2DNode, or TAbstractTexture3DNode.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this.
      Exception: you can mix it with chVisibleVRML1State or
      chTextureRendererProperties. }
    chTextureImage,

    { Texture properties used by the renderer changed (something other than
      only the texture data). This is for fields contained in X3DTextureNode.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this.
      Exception: you can mix it with chTextureImage. }
    chTextureRendererProperties,

    { Texture properties inside TextureProperties node changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chTexturePropertiesNode,

    { What is considered a shadow caster changed.

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chShadowCasters,

    { Mark the generated texture node (parent of this field) as requiring update
      (assuming it's "update" field value wants it too).

      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chGeneratedTextureUpdateNeeded,

    { VRML >= 2.0 FontStyle changed.
      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chFontStyle,

    { HeadLight on status changed.
      Caller will analyze the scene to know what this implicates,
      don't include other flags with this. }
    chHeadLightOn,

    { Clip plane visible change (enabled or plane equation). }
    chClipPlane,

    { Enabled field of the pointing-device drag sensor changed.
      Use only for TSFBool fields within TAbstractDragSensorNode. }
    chDragSensorEnabled,

    { NavigationInfo field value used in
      TCastleSceneCore.NavigationTypeFromNavigationInfo or
      TCastleSceneCore.CameraFromNavigationInfo
      changed. }
    chNavigationInfo,

    { ScreenEffect.enabled changed }
    chScreenEffectEnabled,

    { X3DBackgroundNode properties that are stored inside TBackground
      display list have changed. }
    chBackground,

    { Everything changed and needs to be recalculated.
      This is needed for changes on stuff internally cached in
      TCastleSceneCore, TCastleScene, TShape that cannot be expressed
      as one of above flags.

      Use only as a last resort, as this is very costly!
      (And in an ideal implementation, should not be needed.)

      Don't include other flags with this. }
    chEverything,

    { Higher-level shadow maps fields changed.
      They have to be processed to lower-level fields by calling
      TCastleSceneCore.ProcessShadowMapsReceivers.

      Don't include other flags with this. }
    chShadowMaps,

    { Shading changed from wireframe to non-wireframe. }
    chWireframe);
  TX3DChanges = set of TX3DChange;

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}
