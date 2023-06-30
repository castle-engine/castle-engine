{%MainUnit x3dfields.pas}
{
  Copyright 2002-2022 Michalis Kamburelis.

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
    Used by @link(TX3DField.ExecuteChange) and @link(TX3DField.ChangeAlways). }
  TX3DChange = (
    { This field has no effect on the rendering/behavior of anything. }
    chNone,

    { Something visible in the geometry changed.
      See vcVisibleGeometry.
      This means that VisibleChangeHere with vcVisibleGeometry included should
      be called.

      See TVisibleChange for the difference between "geometry" and "non-geometry"
      change. In short, "geometry" change means that shape of something changed,
      so even rendering it with some special shader (e.g. only to depth buffer)
      is affected (makes different result). }
    chVisibleGeometry,

    { Something visible changed, but not geometry.
      See vcVisibleNonGeometry.
      This means that VisibleChangeHere with vcVisibleNonGeometry included should
      be called. }
    chVisibleNonGeometry,

    { Call VisibleChangeHere to redisplay the scene.
      In contrast to chVisibleGeometry or chVisibleNonGeometry,
      this flag should be used if your change requires
      redisplay of the 3D view for some other reason
      (e.g. need to update rendered texture). }
    chRedisplay,

    { Transformation of children of this node changed. }
    chTransform,

    { Coordinate node (both VRML 1.0 and >= 2.0) "point" field changed. }
    chCoordinate,

    { Normal node (both VRML 1.0 and >= 2.0) "vector" field changed. }
    chNormal,

    { Tangent node "vector" field changed. }
    chTangent,

    { Something visible in VRML 1.0 state node (that may be present
      in TX3DGraphTraverseState.VRML1State) changed, but not geometry.

      - Do not call this for Coordinate node change
        (this one should go through chCoordinate only, even for VRML 1.0).
        Actually Coordinate node change would be chGeometryVRML1State anyway.

      - Do not call this for Normal node change
        (this one should go through chNormal only, even for VRML 1.0).

      - Do not call this for changes that don't require recreating Proxy,
        and only require redisplay (chVisibleNonGeometry is enough for them).

      This is allowed, and ignored, on nodes that are not part of VRML 1.0
      state. (This is useful for alphaChannel field, that is declared
      in TAbstractGeometryNode, and so is part of some VRML 1.0 state nodes
      but is also part of VRML >= 2.0 nodes.) }
    chVisibleVRML1State,

    { Some visible geometry changed because of VRML 1.0 state node change.
      This is for VRML 1.0 state node changes, excluding
      - non-geometry changes (these go to chVisibleVRML1State)
      - Coordinate changes (these go to chCoordinate)
      - Normal changes (these go to chNormal, they are non-geometry anyway). }
    chGeometryVRML1State,

    { Light property that is also reflected in TLightInstance structure.
      Only allowed on node's descending from TAbstractLightNode. }
    chLightInstanceProperty,

    { Light's location and/or direction changed.
      Implies also chLightInstanceProperty change. }
    chLightLocationDirection,

    { TCastleSceneCore.InternalMainLightForShadows possibly changed because of this change. }
    chLightForShadowVolumes,

    { Switch.whichChoice changed, for VRML >= 2.0. }
    chSwitch2,

    { X3DColorNode colors changed. }
    chColorNode,

    { X3DTextureCoordinateNode coords changed. }
    chTextureCoordinate,

    { VRML >= 2.0 TextureTransform changed.
      Not for multi-texture node changes, only the simple nodes changes. }
    chTextureTransform,

    { Geometry node visible (or collidable) changed.
      Use for fields of the geometry node. }
    chGeometry,

    { Like chGeometry and in addition font texture changed. Only on TTextNode. }
    chGeometryFontChanged,

    { X3DEnvironmentalSensorNode bounds (size/center) changed. }
    chEnvironmentalSensorBounds,

    { TimeDependent node is start/stop/pause/resume time changed. }
    chTimeStopStart,

    { Viewpoint vectors (position, direction, up, gravity up) changed. }
    chViewpointVectors,

    { Viewpoint projection changed. }
    chViewpointProjection,

    { Texture image (data) needs reloading (url or source SFImage
      data changed). This is for TAbstractTexture2DNode, or TAbstractTexture3DNode. }
    chTextureImage,

    { Texture properties used by the renderer changed (something other than
      only the texture data). This is for fields contained in X3DTextureNode. }
    chTextureRendererProperties,

    { Texture properties inside TextureProperties node changed. }
    chTexturePropertiesNode,

    { What is considered a shadow caster changed. }
    chShadowCasters,

    { Mark the generated texture node (parent of this field) as requiring update
      (assuming it's "update" field value wants it too). }
    chGeneratedTextureUpdateNeeded,

    { VRML 2.0 / X3D FontStyle changed, and it requires recreating TTextNode
      geometry (but not texture).
      The node must be TFontStyleNode. }
    chFontStyle,

    { VRML 2.0 / X3D FontStyle changed, and it requires recreating TTextNode
      geometry and texture (so underlying font changed).
      The node must be TFontStyleNode. }
    chFontStyleFontChanged,

    { HeadLight on status changed. }
    chHeadLightOn,

    { Clip plane visible change (enabled or plane equation). }
    chClipPlane,

    { Enabled field of the pointing-device drag sensor changed.
      Use only for TSFBool fields within TAbstractDragSensorNode. }
    chDragSensorEnabled,

    { NavigationInfo field value used in
      NavigationTypeFromNavigationInfo or
      TCastleSceneCore.InternalUpdateNavigation changed. }
    chNavigationInfo,

    { ScreenEffect.enabled changed. }
    chScreenEffectEnabled,

    { X3DBackgroundNode properties that are stored inside TBackground
      display list have changed. }
    chBackground,

    { Everything changed and needs to be recalculated.
      This is needed for changes on stuff internally cached in
      TCastleSceneCore, TCastleScene, TShape that cannot be expressed
      as one of above flags.

      Use only as a last resort, as this is very costly!
      (And in an ideal implementation, should not be needed.) }
    chEverything,

    { Higher-level shadow maps fields changed.
      They have to be processed to lower-level fields by calling
      TCastleSceneCore.ProcessShadowMapsReceivers. }
    chShadowMaps,

    { Shading changed from wireframe to non-wireframe. }
    chWireframe,

    { Children of X3D (or VRML 2) grouping node have been added or removed. }
    chGroupChildren,

    { Children of X3D (or VRML 2) grouping node have been added (but not removed).
      This is special case of chGroupChildren.
      If unsure whether something was removed or not, it is always safe to use chGroupChildren.
      If you are sure that only additions were done, use chGroupChildrenAdd to be more efficient. }
    chGroupChildrenAdd,

    { Shape.bboxCenter/Size changed, so TShape.LocalBoundingBox changed. }
    chBBox
  );
  TX3DChanges = set of TX3DChange;

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}
