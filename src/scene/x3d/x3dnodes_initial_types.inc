{%MainUnit x3dnodes.pas}
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

  { Forward declarations } { }
  TX3DNodeList = class;
  TX3DNode = class;
  TAbstractMetadataNode = class;
  TCoordinate3Node_1 = class;
  TShapeHintsNode_1 = class;
  TFontStyleNode_1 = class;
  TMaterialNode_1 = class;
  TMaterialBindingNode_1 = class;
  TNormalNode = class;
  TNormalBindingNode_1 = class;
  TTexture2Node_1 = class;
  TTextureCoordinate2Node_1 = class;
  TAbstractGeometryNode = class;
  TAbstractLightNode = class;
  TAbstractShapeNode = class;
  TAbstractTexture2DNode = class;
  TBlendModeNode = class;
  TAbstractTextureNode = class;
  TX3DEventsEngine = class;
  TClipPlaneNode = class;
  THAnimHumanoidNode = class;
  TLocalFogNode = class;
  TEffectNode = class;
  TX3DRootNode = class;
  TX3DGraphTraverseState = class;
  TPointingDeviceSensorList = class;
  TMaterialInfo = class;
  TSFNode = class;
  TMFNode = class;
  TX3DPrototypeNode = class;
  TX3DPrototypeBase = class;
  TX3DPrototypeBaseList = class;
  TX3DRoute = class;
  TX3DRouteList = class;
  TX3DImport = class;
  TX3DExport = class;
  TX3DInterfaceDeclaration = class;
  TX3DNodeNames = class;
  TX3DReaderNames = class;
  TX3DPrototypeNames = class;
  TX3DInterfaceDeclarationList = class;
  TX3DGraphTraverseStateStack = class;
  TShapeNode = class;
  TTransformNode = class;
  TAbstractCoordinateNode = class;
  TAbstractNormalNode = class;
  TFogCoordinateNode = class;
  TAbstractTextureCoordinateNode = class;
  TAbstractEnvironmentTextureNode = class;
  TAbstractPunctualLightNode = class;
  TAbstractColorNode = class;
  TTexturePropertiesNode = class;
  TAbstractTextureTransformNode = class;
  TAbstractSingleTextureCoordinateNode = class;
  TTangentNode = class;
  TTransformFunctionality = class;
  TTimeDependentFunctionality = class;
  TGeneratedTextureFunctionality = class;
  TX3DExportList = class;

  TX3DNodeClass = class of TX3DNode;

  TX3DNodeProc = procedure (Node: TX3DNode) of object;
  TX3DNodeSearchProc = function (Node: TX3DNode): Pointer of object;

  PTraversingInfo = ^TTraversingInfo;
  TTraversingInfo = record
    Node: TX3DNode;
    ParentInfo: PTraversingInfo;
  end;

  { Used as a callback by TX3DNode.Traverse. }
  TTraversingFunc = function (Node: TX3DNode;
    StateStack: TX3DGraphTraverseStateStack;
    ParentInfo: PTraversingInfo;
    var TraverseIntoChildren: boolean): Pointer of object;

  TTraversingAfterFunc = procedure (Node: TX3DNode;
    StateStack: TX3DGraphTraverseStateStack;
    ParentInfo: PTraversingInfo) of object;

  TEnumerateChildrenFunction =
    function (Node, Child: TX3DNode): Pointer of object;

  TEnumerateReplaceNodesFunction =
    procedure (ParentNode: TX3DNode; var Node: TX3DNode) of object;

  TX3DAccessType = (atInputOnly, atOutputOnly, atInitializeOnly, atInputOutput);
  TX3DAccessTypes = set of TX3DAccessType;

  TNodeTransformationChange = (
    ntcNone,
    ntcSwitch, //< TSwitchNode
    ntcLOD, //< TLODNode
    ntcTransform, //< anything with TTransformFunctionality
    ntcGeometry, //< TAbstractGeometryNode
    ntcBackground, //< TAbstractBackgroundNode
    ntcFog, //< TFogNode
    ntcViewpoint, //< TAbstractViewpointNode
    ntcLight, //< TAbstractLightNode
    ntcProximitySensor, //< TProximitySensorNode
    ntcVisibilitySensor //< TVisibilitySensorNode
  );

  TX3DVersion = CastleInternalX3DLexer.TX3DVersion;
  TX3DEncoding = CastleInternalX3DLexer.TX3DEncoding;

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}
