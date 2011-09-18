{ -*- compile-command: "./compile_console.sh" -*- }
{
  Copyright 2004-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestVRMLNodes;

{$I tests.inc}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, VectorMath, VRMLNodes;

type
  TTestVRMLNodes = class(TTestCase)
  published
    procedure TestNodesManager;

    { This is really large test that reads and writes various VRML files
      and checks whether the generated VRML file is the same.
      It checks "the same" by comparing sequence of VRMLLexer
      tokens for them.

      Note that this is not guaranteed to pass for every file,
      since writing is allowed to change some things.
      But we test only on subset of VRML files that are known to pass this.

      So this checks both reading and writing of VRML files.
    }
    procedure TestParseSaveToFile;

    procedure TestUniqueFields;
    procedure TestInterfaceSupports;
    procedure TestAllowedChildren;
    procedure TestContainerFieldList;
    procedure TestContainerFieldGeometry;
    procedure TestDestructionNotification;

    procedure TestGeometryNodesImplemented;

    { Test all geometry nodes should have Changes = [chGeometry]
      on all fields (except "metadata").
      All non-geometry nodes should not have chGeometry on any field. }
    procedure TestGeometryNodesChanges;

    { All VRML 1 state nodes (except Coordinate)
      should have Changes = [chVisibleVRML1State] (and possibly more,
      like chUseBlending) }
    procedure TestVisibleVRML1StateChanges;

    { All Color nodes should have Changes = [chColorNode] }
    procedure TestColorNodeChanges;

    { All tex coord nodes should have Change = [chTextureCoordinate] }
    procedure TestTextureCoordinate;

    { Detect if we consciously set Changes (and ChangesAlways)
      on all fields correctly.

      By default, ChangesAlways is [] and Changes
      return it, so there could be a chance that some field are left with
      [] by accident. This checks all the fields with Changes = [],
      they *must* be added to ConfirmedEmptyChanges function. }
    procedure TestEmptyChanges;

    { Try calling GetTimeDependentNodeHandler
      on every IAbstractTimeDependentNode, and use the handler.
      Catches e.g. not overriden CycleInterval. }
    procedure TestTimeDependentNodeHandlerAvailable;

    procedure TestITransformNode;
    procedure TestSortPositionInParent;
    procedure TestRootNodeMeta;
    procedure TestConvertToX3D;
    procedure TestReadingWritingQuotes;
  end;

implementation

uses KambiUtils, VRMLLexer, KambiClassUtils, KambiFilesUtils, VRMLFields,
  KambiTimeUtils, FGL {$ifdef VER2_2}, FGLObjectList22 {$endif};

{ TNode* ------------------------------------------------------------ }

type
  TSpecialNode = class(TX3DNode)
    function NodeTypeName: string; override;
  end;

function TSpecialNode.NodeTypeName: string;
begin
 result := 'OohImSoSpecial';
end;

type
  TSomethingNode = class(TX3DNode)
    class function ClassNodeTypeName: string; override;
  end;

class function TSomethingNode.ClassNodeTypeName: string;
begin
 result := 'WellImNothingSpecial';
end;

{ ------------------------------------------------------------ }

procedure TTestVRMLNodes.TestNodesManager;
begin
 try
  { throw exception because TSpecialNode.ClassNodeTypeName = '' }
  NodesManager.RegisterNodeClass(TSpecialNode);
  raise Exception.Create('NodesManager.RegisterNodeClass(TSpecialNode); SHOULD throw exception');
 except on ENodesManagerError do ; end;

 try
  { throw exception because TFogNode is already registered }
  NodesManager.RegisterNodeClass(TFogNode);
  raise Exception.Create('NodesManager.RegisterNodeClass(TFogNode); SHOULD throw exception');
 except on ENodesManagerError do ; end;

 try
  { this should succeed }
  NodesManager.RegisterNodeClass(TSomethingNode);
 finally
  NodesManager.UnRegisterNodeClass(TSomethingNode);
 end;
end;

{ TVRMLTokenInfo and TVRMLTokenInfoList ---------------------------------- }

type
  TVRMLTokenInfo = class
    Token: TVRMLToken;
    Float: Float; //< for both vtFloat and vtInteger
    Name: string; //< for vtName
    AString: string; //< for vtString
    Keyword: TVRMLKeyword; //< for vtKeyword
    Integer: Int64; //< for vtInteger
  end;
  TVRMLTokenInfoList = class(specialize TFPGObjectList<TVRMLTokenInfo>)
    procedure AssertEqual(SecondValue: TVRMLTokenInfoList);
    procedure ReadFromFile(const FileName: string);
  end;

procedure TVRMLTokenInfoList.AssertEqual(
  SecondValue: TVRMLTokenInfoList);

  procedure AssertEqualTokens(const T1, T2: TVRMLTokenInfo);

    function DescribeTokenInfo(const T: TVRMLTokenInfo): string;
    const
      VRMLTokenNames: array[TVRMLToken]of string = (
        'keyword', 'name',
        '"{"', '"}"', '"["', '"]"', '"("', '")"', '"|"', '","', '"."', '":"',
        'float', 'integer', 'string', 'end of stream');
    begin
      Result := VRMLTokenNames[T.Token];
      case T.Token of
        vtKeyword: result := result +' "' +VRMLKeywords[T.Keyword]+'"';
        vtName: result := '"' +T.Name+'"';
        vtFloat: result := result +' ' +FloatToStr(T.Float);
        vtInteger: result := result +' ' +IntToStr(T.Integer);
        vtString: result := result+' "'+T.AString+'"';
      end;
    end;

  begin
    Assert(
      (T1.Token = T2.Token) and
      ( (T1.Token <> vtKeyword) or (T1.Keyword = T2.Keyword) ) and
      ( (T1.Token <> vtName) or
        (T1.Name = T2.Name) or
        { route events from exposed fields may be resolved on save }
        (T1.Name = 'set_' + T2.Name) or
        (T2.Name = 'set_' + T1.Name) or
        (T1.Name = T2.Name + '_changed') or
        (T2.Name = T1.Name + '_changed')
      ) and
      ( (T1.Token <> vtFloat) or (T1.Float = T2.Float) ) and
      ( (T1.Token <> vtInteger) or ( (T1.Float = T2.Float) and
                                        (T1.Integer = T2.Integer) ) ) and
      ( (T1.Token <> vtString) or (T1.AString = T2.AString) ),
      Format('VRML tokens different: %s and %s',
        [DescribeTokenInfo(T1), DescribeTokenInfo(T2)]));
  end;

var
  I: Integer;
begin
  Assert(Count = SecondValue.Count, Format(
    'TVRMLTokenInfoList.Equal: different counts %d and %d',
    [Count, SecondValue.Count]));
  for I := 0 to Count - 1 do
    AssertEqualTokens(Items[I], SecondValue[I]);
end;

{ Note that this can be used to test correctly only files that can
  be correctly parsed by pure Lexer.NextToken calls. All valid VRML >= 2.0
  files are like that, although parser in practice has to use NextTokenForceXxx
  methods because of unfortunately
  1. invalid VRML files (that use some funny node names)
  2. VRML 1.0 ugly feature that string doesn't have to be enclosed in "" }
procedure TVRMLTokenInfoList.ReadFromFile(const FileName: string);
var
  Lexer: TVRMLLexer;

  function CurrentToken: TVRMLTokenInfo;
  begin
    Result.Token := Lexer.Token;
    Result.Keyword := Lexer.TokenKeyword;
    Result.Name := Lexer.TokenName;
    Result.Float := Lexer.TokenFloat;
    Result.Integer := Lexer.TokenInteger;
    Result.AString := Lexer.TokenString;
  end;

var
  I: Integer;
begin
  Count := 0;
  Lexer := TVRMLLexer.CreateFromFile(FileName);
  try
    Add(CurrentToken);
    while Lexer.Token <> vtEnd do
    begin
      Lexer.NextToken;
      Add(CurrentToken);
    end;
  finally FreeAndNil(Lexer); end;
end;

procedure TTestVRMLNodes.TestParseSaveToFile;

  procedure TestReadWrite(const FileName: string);
  var
    First, Second: TVRMLTokenInfoList;
    Node: TX3DNode;
    S: TMemoryStream;
    SPeek: TPeekCharStream;
    NewFile: string;
  begin
    First := nil;
    Second := nil;
    Node := nil;
    try
      First := TVRMLTokenInfoList.Create;
      First.ReadFromFile(FileName);

      Node := LoadVRMLClassic(FileName, false);
      NewFile := GetTempPath + 'test_castle_game_engine.wrl';
      SaveVRML(Node, NewFile, ProgramName, '', xeClassic, false);

      Second := TVRMLTokenInfoList.Create;
      Second.ReadFromFile(NewFile);

      First.AssertEqual(Second);
    finally
      FreeAndNil(First);
      FreeAndNil(Second);
      FreeAndNil(Node);
    end;
  end;

begin
  {$ifdef CASTLE_ENGINE_TRUNK_AVAILABLE}
  TestReadWrite('../../demo_models/x3d/proto_sfnode_default.x3dv');
  TestReadWrite('../../demo_models/x3d/tricky_def_use.x3dv');
  {$endif CASTLE_ENGINE_TRUNK_AVAILABLE}
end;

procedure TTestVRMLNodes.TestInterfaceSupports;
var
  L: TX3DNodeClassesList;

  function IndexOfAnyAncestorByClass(C: TX3DNodeClass): boolean;
  var
    N: TX3DNode;
  begin
    N := C.Create('', '');
    try
      Result := L.IndexOfAnyAncestor(N) <> -1;
    finally FreeAndNil(N) end;
  end;

begin
  { When our interfaces have appropriate GUIDs, "Supports" works Ok. }

  Assert(Supports(TGroupNode_2, IAbstractChildNode));
  Assert(Supports(TSwitchNode_2, IAbstractChildNode));
  Assert(not Supports(TConeNode_2, IAbstractChildNode));
  Assert(not Supports(TAppearanceNode, IAbstractChildNode));
  Assert(not Supports(TX3DNode, IAbstractChildNode));
  Assert(not Supports(TObject, IAbstractChildNode));

  L := TX3DNodeClassesList.Create;
  try
    L.AddRegisteredImplementing(IAbstractChildNode);
    { similar to above tests, but now using L.IndexOfAnyAncestor.
      So we test IndexOfAnyAncestor and AddRegisteredImplementing,
      AddRegisteredImplementing also uses "Supports" under the hood
      and results should be the same. }
    Assert(IndexOfAnyAncestorByClass(TGroupNode_2));
    Assert(IndexOfAnyAncestorByClass(TSwitchNode_2));
    Assert(not IndexOfAnyAncestorByClass(TConeNode_2));
    Assert(not IndexOfAnyAncestorByClass(TAppearanceNode));
    Assert(not IndexOfAnyAncestorByClass(TX3DNode));
  finally FreeAndNil(L) end;
end;

procedure TTestVRMLNodes.TestUniqueFields;
var
  I, J, K: Integer;
  N: TX3DNode;
  CurrentName: string;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create('', '');
    try

      { Writeln(N.NodeTypeName, ' ', Supports(N, IAbstractChildNode)); }

      { Test that all fields, events names are different.

        Doesn't detect if two alternative names match each other for now!
        (Although will detect if some alternative name will match non-alternative
        name, since uses IsName comparison).

        Also, doesn't check the implicitly exposed events for now. }

      for J := 0 to N.Fields.Count - 1 do
      begin
        CurrentName := N.Fields[J].Name;
        for K := 0 to N.Fields.Count - 1 do
          Assert((K = J) or (not N.Fields[K].IsName(CurrentName)));
        for K := 0 to N.Events.Count - 1 do
          Assert(not N.Events[K].IsName(CurrentName));
      end;

      for J := 0 to N.Events.Count - 1 do
      begin
        CurrentName := N.Events[J].Name;
        for K := 0 to N.Fields.Count - 1 do
          Assert(not N.Fields[K].IsName(CurrentName));
        for K := 0 to N.Events.Count - 1 do
          Assert((K = J) or (not N.Events[K].IsName(CurrentName)));
      end;
    finally FreeAndNil(N) end;
  end;
end;

procedure TTestVRMLNodes.TestAllowedChildren;
var
  AllowedChildrenNodes: TX3DNodeClassesList;
  AllowedGeometryNodes: TX3DNodeClassesList;
  I: Integer;
  N: TX3DNode;
begin
  { AllowedChildrenNodes and AllowedGeometryNodes were written before X3D
    transition. I didn't then check AllowedChildren using some general
    inheritance classes, like X3D, but I had simply long lists for
    some properties.

    They were removed from VRMLNodes, since using X3D inheritance
    is obiously much simpler and long-term solution. For example,
    all children nodes simply inherit from TAbstractChildNode
    (actually, IAbstractChildNode, and since FPC "Supports" doesn't work
    we simply have IAbstractChildNode_Descendants lists... still, it's much
    shorter list than AllowedChildrenNodes).
    This avoids the need to maintain long lists like these below, that would be
    nightmare considering large number of X3D nodes.

    Still, since I already wrote these lists, they are used below
    for testing. To make sure X3D inheritance didn't break any
    previous behavior, all classes on AllowedChildrenNodes must inherit
    from TAbstractChildNode, and similat for geometry nodes. }

  AllowedChildrenNodes := TX3DNodeClassesList.Create;
  AllowedChildrenNodes.AssignArray([
    { We add all nodes for VRML < 2.0, because we allow
      to mix VRML 1.0 inside VRML 2.0. }

    { Inventor spec nodes }
    TIndexedTriangleMeshNode_1, TRotationXYZNode,

    { VRML 1.0 spec nodes }
    TAsciiTextNode_1, TConeNode_1, TCubeNode_1, TCylinderNode_1,
    TIndexedFaceSetNode_1, TIndexedLineSetNode_1,
    TPointSetNode_1, TSphereNode_1,
    TCoordinate3Node, TFontStyleNode_1, TInfoNode, TLODNode_1, TMaterialNode_1,

    { TNormalNode used to also be allowed here, but it's also used by X3D,
      and I don't want to mess X3D inheritance by making TNormalNode descendant
      of IAbstractChildNode --- which is required only when you mix VRML 1.0
      and X3D... When mixing VRML 1.0 and VRML >= 2.0 in a singe file,
      you will have to live with warnings about Normal not allowed as
      children in VRML >= 2.0 nodes.

      Also TTexture2Node was here, but is removed: I don't want to mess
      X3D inheritance just for VRML 1.0 + 2.0 mixing feature. }

    TMaterialBindingNode, TNormalBindingNode,
    TTexture2TransformNode,
    TTextureCoordinate2Node, TShapeHintsNode,
    TMatrixTransformNode_1, TRotationNode,
    TScaleNode, TTransformNode_1,
    TTranslationNode,
    TOrthographicCameraNode, TPerspectiveCameraNode,
    TDirectionalLightNode_1, TPointLightNode_1, TSpotLightNode_1,
    TGroupNode_1, TSeparatorNode, TSwitchNode_1, TTransformSeparatorNode,
    TWWWAnchorNode,
    TWWWInlineNode,

    { Kambi non-standard nodes }
    TKambiTriangulationNode,
    TKambiHeadLightNode,
    //TText3DNode,
    //TBlendModeNode,
    //TKambiAppearanceNode,

    { VRML 2.0 spec nodes }
    TAnchorNode,
    //TAppearanceNode,
    //TAudioClipNode,
    TBackgroundNode,
    TBillboardNode,
    //TBoxNode,
    TCollisionNode,
    //TColorNode,
    TColorInterpolatorNode,
    //TConeNode,
    //TContour2DNode,
    //TCoordinateNode,
    { VRML 2.0 spec section "4.6.5 Grouping and children nodes"
      doesn't say is CoordinateDeformer allowed or not as children node.
      To be fixed when I'll implement CoordinateDeformer handling. }
    TCoordinateDeformerNode,
    TCoordinateInterpolatorNode,
    //TCylinderNode,
    TCylinderSensorNode,
    TDirectionalLightNode,
    //TElevationGridNode,
    //TExtrusionNode,
    TFogNode,
    { VRML 2.0 spec section "4.6.5 Grouping and children nodes"
      doesn't say is TFontStyleNode allowed as children node,
      but FontStyle docs say that it's only for Text.fontStyle. }
    //TFontStyleNode,
    //TGeoCoordinateNode,
    //TGeoElevationGridNode,
    TGeoLocationNode,
    TGeoLODNode,
    TGeoMetadataNode,
    //TGeoOriginNode,
    TGeoPositionInterpolatorNode,
    TGeoTouchSensorNode,
    TGeoViewpointNode,
    TGroupNode,
    //TImageTextureNode,
    //TIndexedFaceSetNode,
    //TIndexedLineSetNode,
    TInlineNode,
    { VRML 2.0 spec doesn't say InlineLoadControl is valid children
      node, it also doesn't say it's not valid. Common sense says
      it's valid. }
    TInlineLoadControlNode,
    TLODNode_2,
    //TMaterialNode,
    //TMovieTextureNode,
    TNavigationInfoNode,
    { Normal node is not a valid children node for VRML 2.0.
      But we don't have separate TNormalNode_1 and TNormalNode classes,
      so node normal was already added here as all other VRML 1.0 nodes.
      So it's allowed children node for us --- in the spirit thst
      we allow to mix VRML 1.0 and 2.0. }
    //{ TNormalNode, - registered already as VRML 1.0 node }
    TNormalInterpolatorNode,
    //TNurbsCurveNode,
    //TNurbsCurve2DNode,
    { VRML 2.0 spec section "4.6.5 Grouping and children nodes"
      doesn't say is NurbsGroup allowed or not as children node.
      To be fixed when I'll implement NurbsGroup handling. }
    TNurbsGroupNode,
    TNurbsPositionInterpolatorNode_2,
    //TNurbsSurfaceNode,
    //TNurbsTextureSurfaceNode,
    TOrientationInterpolatorNode,
    { VRML 2.0 spec section "4.6.5 Grouping and children nodes"
      doesn't say is PixelTexture allowed or not as children node.
      But common sense says it's only for Appearance.texture field. }
    //TPixelTextureNode,
    TPlaneSensorNode,
    TPointLightNode,
    //TPointSetNode,
    //TPolyline2DNode,
    TPositionInterpolatorNode,
    TProximitySensorNode,
    TScalarInterpolatorNode,
    TScriptNode,
    TShapeNode,
    TSoundNode,
    //TSphereNode,
    TSphereSensorNode,
    TSpotLightNode,
    TSwitchNode,
    //TTextNode,
    //TTextureCoordinateNode,
    //TTextureTransformNode,
    TTimeSensorNode,
    TTouchSensorNode,
    TTransformNode,
    //TTrimmedSurfaceNode,
    TViewpointNode,
    TVisibilitySensorNode,
    TWorldInfoNode,

    { X3D nodes }
    //TComposedShaderNode,
    //TPackagedShaderNode,
    //TProgramShaderNode,
    //TShaderPartNode,
    //TShaderProgramNode
    TSwitchNode,
    TLODNode
  ]);

  AllowedGeometryNodes := TX3DNodeClassesList.Create;
  AllowedGeometryNodes.AssignArray([
    TBoxNode,
    TConeNode,
    TContour2DNode,
    TCylinderNode,
    TElevationGridNode,
    TExtrusionNode,
    TGeoElevationGridNode,
    TIndexedFaceSetNode,
    TIndexedLineSetNode,
    TNurbsCurveNode,
    TNurbsSurfaceNode,
    TPointSetNode,
    TSphereNode,
    TTextNode,
    TText3DNode,
    TTrimmedSurfaceNode
  ]);

  try
    for I := 0 to AllowedChildrenNodes.Count - 1 do
    try
      Assert(Supports(AllowedChildrenNodes[I], IAbstractChildNode));

      { Just to make sure, check also the created class
        (I don't trust FPC interfaces for now...) }
      N := AllowedChildrenNodes[I].Create('', '');
      try
        Assert(Supports(N, IAbstractChildNode));
      finally FreeAndNil(N) end;
    except
      on E: Exception do
      begin
        Writeln('Failed on ', AllowedChildrenNodes[I].ClassName, ' is IAbstractChildNode');
        raise;
      end;
    end;

    for I := 0 to AllowedGeometryNodes.Count - 1 do
    try
      Assert(AllowedGeometryNodes[I].InheritsFrom(TAbstractX3DGeometryNode));
    except
      on E: Exception do
      begin
        Writeln('Failed on ', AllowedGeometryNodes[I].ClassName, ' is TAbstractX3DGeometryNode');
        raise;
      end;
    end;

  finally
    FreeAndNil(AllowedGeometryNodes);
    FreeAndNil(AllowedChildrenNodes);
  end;
end;

procedure TTestVRMLNodes.TestContainerFieldList;
const
  { This is pasted, and then processed by regexps, from X3D XML
    encoding specification (chapter 6 "Encoding of nodes").

    This allows me to easily test (and see at a glance all errors)
    all containerField values. And when fixing the containerField values,
    I can run the test once again to see I didn't break anything...
    just perfect. }
  ContainerFieldStr =
    'Anchor=children' + NL +
    { There's a bug about this in X3D spec, see Appearance node implementation comments. }
    'Appearance=appearance' + NL +
    'Arc2D=geometry' + NL +
    'ArcClose2D=geometry' + NL +
    { There's a bug about this in X3D spec, see Appearance node implementation comments. }
    'AudioClip=source' + NL +
    'Background=children' + NL +
    'BallJoint=joints' + NL +
    'Billboard=children' + NL +
    'BooleanFilter=children' + NL +
    'BooleanSequencer=children' + NL +
    'BooleanToggle=children' + NL +
    'BooleanTrigger=children' + NL +
    'BoundedPhysicsModel=physics' + NL +
    'Box=geometry' + NL +
    'CADAssembly=children' + NL +
    'CADFace=children' + NL +
    'CADLayer=children' + NL +
    'CADPart=children' + NL +
    'Circle2D=geometry' + NL +
    { There's a bug about this in X3D spec, see ClipPlane node implementation comments. }
    'ClipPlane=children' + NL +
    'CollidableOffset=children' + NL +
    'CollidableShape=children' + NL +
    'Collision=children' + NL +
    'CollisionCollection=children' + NL +
    'CollisionSensor=children' + NL +
    'CollisionSpace=children' + NL +
    'Color=color' + NL +
    'ColorDamper=children' + NL +
    'ColorInterpolator=children' + NL +
    'ColorRGBA=color' + NL +
    'ComposedCubeMapTexture=texture' + NL +
    'ComposedShader=shaders' + NL +
    'ComposedTexture3D=texture' + NL +
    'Cone=geometry' + NL +
    'ConeEmitter=emitter' + NL +
    'Contact=children' + NL +
    'Contour2D=trimmingContour' + NL +
    'ContourPolyline2D=geometry' + NL +
    'Coordinate=coord' + NL +
    'CoordinateDamper=children' + NL +
    'CoordinateDouble=coord' + NL +
    'CoordinateInterpolator=children' + NL +
    'CoordinateInterpolator2D=children' + NL +
    'Cylinder=geometry' + NL +
    'CylinderSensor=children' + NL +
    'DirectionalLight=children' + NL +
    'DISEntityManager=children' + NL +
    'DISEntityTypeMapping=children' + NL +
    'Disk2D=geometry' + NL +
    'DoubleAxisHingeJoint=joints' + NL +
    'EaseInEaseOut=children' + NL +
    'ElevationGrid=geometry' + NL +
    'EspduTransform=children' + NL +
    'ExplosionEmitter=emitter' + NL +
    'Extrusion=geometry' + NL +
    'FillProperties=fillProperties' + NL +
    'FloatVertexAttribute=attrib' + NL +
    'Fog=children' + NL +
    'FogCoordinate=fogCoord' + NL +
    'FontStyle=fontStyle' + NL +
    'ForcePhysicsModel=physics' + NL +
    'GeneratedCubeMapTexture=texture' + NL +
    'GeoCoordinate=coord' + NL +
    'GeoElevationGrid=geometry' + NL +
    'GeoLocation=children' + NL +
    'GeoLOD=children' + NL +
    'GeoMetadata=children' + NL +
    'GeoOrigin=geoOrigin' + NL +
    'GeoPositionInterpolator=children' + NL +
    'GeoTouchSensor=children' + NL +
    'GeoTransform=children' + NL +
    'GeoViewpoint=children' + NL +
    'Group=children' + NL +
    'HAnimDisplacer=displacers' + NL +
    'HAnimHumanoid=children' + NL +
    'HAnimJoint=children' + NL +
    'HAnimSegment=children' + NL +
    'HAnimSite=children' + NL +
    'ImageCubeMapTexture=texture' + NL +
    'ImageTexture=texture' + NL +
    'ImageTexture3D=texture' + NL +
    'IndexedFaceSet=geometry' + NL +
    'IndexedLineSet=geometry' + NL +
    'IndexedQuadSet=geometry' + NL +
    'IndexedTriangleFanSet=geometry' + NL +
    'IndexedTriangleSet=geometry' + NL +
    'IndexedTriangleStripSet=geometry' + NL +
    'Inline=children' + NL +
    'IntegerSequencer=children' + NL +
    'IntegerTrigger=children' + NL +
    'KeySensor=children' + NL +
    'Layer=layers' + NL +
    'LayerSet=children' + NL +
    'Layout=children' + NL +
    'LayoutGroup=children' + NL +
    'LayoutLayer=layers' + NL +
    'LinePickSensor=children' + NL +
    'LineProperties=lineProperties' + NL +
    'LineSet=geometry' + NL +
    'LoadSensor=children' + NL +
    'LocalFog=children' + NL +
    'LOD=children' + NL +
    'Material=material' + NL +
    'Matrix3VertexAttribute=attrib' + NL +
    'Matrix4VertexAttribute=attrib' + NL +
    'MetadataDouble=metadata' + NL +
    'MetadataFloat=metadata' + NL +
    'MetadataInteger=metadata' + NL +
    'MetadataSet=metadata' + NL +
    'MetadataString=metadata' + NL +
    'MotorJoint=joints' + NL +
    { There's a bug about this in X3D spec, claims that MovieTexture
      should have "children" but it's nonsense, MovieTexture is not a child node,
      it should have "texture". }
    'MovieTexture=texture' + NL +
    'MultiTexture=texture' + NL +
    'MultiTextureCoordinate=texCoord' + NL +
    'MultiTextureTransform=textureTransform' + NL +
    'NavigationInfo=children' + NL +
    'Normal=normal' + NL +
    'NormalInterpolator=children' + NL +
    'NurbsCurve=geometry' + NL +
    'NurbsCurve2D=geometry' + NL +
    'NurbsOrientationInterpolator=children' + NL +
    'NurbsPatchSurface=geometry' + NL +
    'NurbsPositionInterpolator=children' + NL +
    'NurbsSet=children' + NL +
    'NurbsSurfaceInterpolator=children' + NL +
    'NurbsSweptSurface=geometry' + NL +
    'NurbsSwungSurface=geometry' + NL +
    'NurbsTextureCoordinate=texCoord' + NL +
    'NurbsTrimmedSurface=geometry' + NL +
    'OrientationChaser=children' + NL +
    'OrientationDamper=children' + NL +
    'OrientationInterpolator=children' + NL +
    'OrthoViewpoint=children' + NL +
    'PackagedShader=shaders' + NL +
    'ParticleSystem=children' + NL +
    'PickableGroup=children' + NL +
    'PixelTexture=texture' + NL +
    'PixelTexture3D=texture' + NL +
    'PlaneSensor=children' + NL +
    'PointEmitter=emitter' + NL +
    'PointLight=children' + NL +
    'PointPicker=children' + NL +
    'PointSet=geometry' + NL +
    'Polyline2D=geometry' + NL +
    'PolylineEmitter=emitter' + NL +
    'Polypoint2D=geometry' + NL +
    'PositionChaser=children' + NL +
    'PositionChaser2D=children' + NL +
    'PositionDamper=children' + NL +
    'PositionDamper2D=children' + NL +
    'PositionInterpolator=children' + NL +
    'PositionInterpolator2D=children' + NL +
    'PrimitivePicker=children' + NL +
    'ProgramShader=shaders' + NL +
    'ProtoInstance=children' + NL +
    'ProximitySensor=children' + NL +
    'QuadSet=geometry' + NL +
    'ReceiverPdu=children' + NL +
    'Rectangle2D=geometry' + NL +
    'RigidBody=bodies' + NL +
    'RigidBodyCollection=children' + NL +
    'ScalarChaser=children' + NL +
    'ScalarInterpolator=children' + NL +
    'ScreenFontStyle=fontStyle' + NL +
    'ScreenGroup=children' + NL +
    'Script=children' + NL +
    'ShaderPart=parts' + NL +
    'ShaderProgram=programs' + NL +
    'Shape=children' + NL +
    'SignalPdu=children' + NL +
    'SingleAxisHingeJoint=joints' + NL +
    'SliderJoint=joints' + NL +
    'Sound=children' + NL +
    'Sphere=geometry' + NL +
    'SphereSensor=children' + NL +
    'SplinePositionInterpolator=children' + NL +
    'SplinePositionInterpolator2D=children' + NL +
    'SplineScalarInterpolator=children' + NL +
    'SpotLight=children' + NL +
    'SquadOrientationInterpolator=children' + NL +
    'StaticGroup=children' + NL +
    'StringSensor=children' + NL +
    'SurfaceEmitter=emitter' + NL +
    'Switch=children' + NL +
    'TexCoordDamper=children' + NL +
    'Text=geometry' + NL +
    'TextureBackground=children' + NL +
    'TextureCoordinate=texCoord' + NL +
    'TextureCoordinate3D=texCoord' + NL +
    'TextureCoordinate4D=texCoord' + NL +
    'TextureCoordinateGenerator=texCoord' + NL +
    'TextureMatrixTransform=textureTransform' + NL +
    { There's a bug in X3D spec here, claims it's lineProperties }
    'TextureProperties=textureProperties' + NL +
    'TextureTransform=textureTransform' + NL +
    'TextureTransform3D=textureTransform' + NL +
    'TimeSensor=children' + NL +
    'TimeTrigger=children' + NL +
    'TouchSensor=children' + NL +
    'Transform=children' + NL +
    'TransformSensor=children' + NL +
    'TransmitterPdu=children' + NL +
    'TriangleFanSet=geometry' + NL +
    'TriangleSet=geometry' + NL +
    'TriangleSet2D=geometry' + NL +
    'TriangleStripSet=geometry' + NL +
    'TwoSidedMaterial=material' + NL +
    'UniversalJoint=joints' + NL +
    'Viewpoint=children' + NL +
    'ViewpointGroup=children' + NL +
    'Viewport=children' + NL +
    'VisibilitySensor=children' + NL +
    'VolumeEmitter=emitter' + NL +
    'VolumePickSensor=children' + NL +
    'WindPhysicsModel=physics' + NL +
    'WorldInfo=children';
var
  ContainerFieldList: TStringList;
  I, Index: Integer;
  N: TX3DNode;
begin
  ContainerFieldList := TStringList.Create;
  try
    ContainerFieldList.Text := ContainerFieldStr;

    { First check that TStringList.IndexOfName works as expected }
    Assert(ContainerFieldList.IndexOfName('WorldInfo') <> -1);
    Assert(ContainerFieldList.IndexOfName('Anchor') <> -1);
    Assert(ContainerFieldList.IndexOfName('NotExisting') = -1);

    for I := 0 to NodesManager.RegisteredCount - 1 do
    begin
      N := NodesManager.Registered[I].Create('', '');
      try
        Index := ContainerFieldList.IndexOfName(N.NodeTypeName);
        if (Index <> -1) and
           (not (N is TFontStyleNode_1)) and
           (not (N is TMaterialNode_1)) then
        try
          Assert(ContainerFieldList.ValueFromIndex[Index] = N.DefaultContainerField);
        except
          on E: Exception do
          begin
            Writeln('Failed on ', N.ClassName, ' containerField, is "',
              N.DefaultContainerField, '", should be "',
              ContainerFieldList.ValueFromIndex[Index], '"');
            raise;
          end;
        end;
      finally FreeAndNil(N) end;
    end;
  finally FreeAndNil(ContainerFieldList) end;
end;

procedure TTestVRMLNodes.TestContainerFieldGeometry;
var
  I: Integer;
  N: TX3DNode;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create('', '');
    try
      if (N is TAbstractGeometryNode) and
         { TContour2DNode is an exception, see TContour2DNode comments.
           It should be treated as non-geometry node for X3D.
           Fortunately, containerField is used only for X3D. }
         (not (N is TContour2DNode)) then
      try
        Assert(N.DefaultContainerField = 'geometry');
      except
        on E: Exception do
        begin
          Writeln('Failed on ', N.ClassName, ' has containerField=geometry');
          raise;
        end;
      end;
    finally FreeAndNil(N) end;
  end;
end;

type
  TMyObject = class
    procedure Foo(Node: TX3DNode);
  end;

procedure TMyObject.Foo(Node: TX3DNode);
begin
end;

procedure TTestVRMLNodes.TestDestructionNotification;
var
  A: TNodeDestructionNotificationList;
  M1, M2, M3: TMyObject;
begin
  A := TNodeDestructionNotificationList.Create;
  M1 := TMyObject.Create;
  M2 := TMyObject.Create;
  M3 := TMyObject.Create;
  try
    A.Add(@M1.Foo);
    A.Add(@M2.Foo);
    A.Add(@M3.Foo);
    Assert(A.IndexOf(@M1.Foo) = 0);
    Assert(A.IndexOf(@M2.Foo) = 1);
    Assert(A.IndexOf(@M3.Foo) = 2);
    A.Remove(@M2.Foo);
    Assert(A.IndexOf(@M1.Foo) = 0);
    Assert(A.IndexOf(@M2.Foo) = -1);
    Assert(A.IndexOf(@M3.Foo) = 1);
  finally
    FreeAndNil(A);
    FreeAndNil(M1);
    FreeAndNil(M2);
    FreeAndNil(M3);
  end;
end;

procedure TTestVRMLNodes.TestGeometryNodesImplemented;
var
  I: Integer;
  N: TX3DNode;
  G, ProxyGeometry: TAbstractGeometryNode;
  State, ProxyState: TVRMLGraphTraverseState;
begin
  State := TVRMLGraphTraverseState.Create;
  try
    for I := 0 to NodesManager.RegisteredCount - 1 do
    begin
      N := NodesManager.Registered[I].Create('', '');
      try
        if N is TAbstractGeometryNode then
        try
          G := TAbstractGeometryNode(N);

          { test proxy may be created }
          ProxyState := State;
          ProxyGeometry := G.Proxy(ProxyState, false);

          { test that methods are overriden correctly, and don't crash }
          G.BoundingBox(State, ProxyGeometry, ProxyState);
          G.LocalBoundingBox(State, ProxyGeometry, ProxyState);
          G.VerticesCount(State, true, ProxyGeometry, ProxyState);
          G.VerticesCount(State, false, ProxyGeometry, ProxyState);
          G.TrianglesCount(State, true, ProxyGeometry, ProxyState);
          G.TrianglesCount(State, false, ProxyGeometry, ProxyState);

          { free proxy temp objects }
          if ProxyGeometry <> nil then FreeAndNil(ProxyGeometry);
          if ProxyState <> State then FreeAndNil(ProxyState);
        except
          on E: Exception do
          begin
            Writeln('Failed on ', N.ClassName, ' implementation');
            raise;
          end;
        end;
      finally FreeAndNil(N) end;
    end;
  finally FreeAndNil(State) end;
end;

procedure TTestVRMLNodes.TestGeometryNodesChanges;
var
  I, J: Integer;
  N: TX3DNode;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create('', '');
    try
      if N is TAbstractGeometryNode then
      begin
        for J := 0 to N.Fields.Count - 1 do
          if N.Fields[J].Name <> 'metadata' then
          try
            Assert(N.Fields[J].Changes = [chGeometry]);
          except
            Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].Name);
            raise;
          end;
      end else
      begin
        for J := 0 to N.Fields.Count - 1 do
        try
          Assert(not (chGeometry in N.Fields[J].Changes));
        except
          Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].Name);
          raise;
        end;
      end

    finally FreeAndNil(N) end;
  end;
end;

procedure TTestVRMLNodes.TestVisibleVRML1StateChanges;
var
  I, J: Integer;
  N: TX3DNode;
  VRML1StateNode: TVRML1StateNode;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create('', '');
    try
      if N.VRML1StateNode(VRML1StateNode) and
         (VRML1StateNode <> vsCoordinate3) then
      begin
        for J := 0 to N.Fields.Count - 1 do
          if (N.Fields[J].Name <> 'metadata') and
             (N.Fields[J].Name <> 'effects') then
          try
            Assert((chVisibleVRML1State in N.Fields[J].Changes) or
                   (chGeometryVRML1State in N.Fields[J].Changes));
          except
            Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].Name);
            raise;
          end;
      end else
      begin
        for J := 0 to N.Fields.Count - 1 do
          { alphaChannel field is allowed exception }
          if N.Fields[J].Name <> 'alphaChannel' then
          try
            Assert(not (chVisibleVRML1State in N.Fields[J].Changes));
            Assert(not (chGeometryVRML1State in N.Fields[J].Changes));
          except
            Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].Name);
            raise;
          end;
      end

    finally FreeAndNil(N) end;
  end;
end;

procedure TTestVRMLNodes.TestColorNodeChanges;
var
  I, J: Integer;
  N: TX3DNode;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create('', '');
    try
      if N is TAbstractColorNode then
      begin
        for J := 0 to N.Fields.Count - 1 do
          if N.Fields[J].Name <> 'metadata' then
          try
            Assert(N.Fields[J].Changes = [chColorNode]);
          except
            Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].Name);
            raise;
          end;
      end else
      begin
        for J := 0 to N.Fields.Count - 1 do
        try
          Assert(not (chColorNode in N.Fields[J].Changes));
        except
          Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].Name);
          raise;
        end;
      end

    finally FreeAndNil(N) end;
  end;
end;

procedure TTestVRMLNodes.TestTextureCoordinate;
var
  I, J: Integer;
  N: TX3DNode;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create('', '');
    try
      if N is TAbstractTextureCoordinateNode then
      begin
        for J := 0 to N.Fields.Count - 1 do
          if N.Fields[J].Name <> 'metadata' then
          try
            Assert(N.Fields[J].Changes = [chTextureCoordinate]);
          except
            Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].Name);
            raise;
          end;
      end else
      begin
        for J := 0 to N.Fields.Count - 1 do
        try
          Assert(not (chTextureCoordinate in N.Fields[J].Changes));
        except
          Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].Name);
          raise;
        end;
      end

    finally FreeAndNil(N) end;
  end;
end;

procedure TTestVRMLNodes.TestEmptyChanges;

  { Confirmed fiels that may have Changes = []. }
  function ConfirmedEmptyChanges(Field: TVRMLField): boolean;

    function FieldIs(Field: TVRMLField;
      const NodeClass: TX3DNodeClass; const FieldName: string): boolean;
    begin
      Result := (Field.ParentNode is NodeClass) and (Field.Name = FieldName);
    end;

  begin
    Result :=
      { Sensors don't affect actual content directly. }
      (Field.ParentNode is TAbstractSensorNode) or
      FieldIs(Field, TTimeSensorNode, 'cycleInterval') or
      FieldIs(Field, TTimeSensorNode, 'enabled') or
      (Field.ParentNode is TWWWAnchorNode) or
      { metadata, info nodes }
      FieldIs(Field, TAbstractNode, 'metadata') or
      (Field.ParentNode is TMetadataDoubleNode) or
      (Field.ParentNode is TMetadataFloatNode) or
      (Field.ParentNode is TMetadataIntegerNode) or
      (Field.ParentNode is TMetadataSetNode) or
      (Field.ParentNode is TMetadataStringNode) or
      (Field.ParentNode is TWorldInfoNode) or
      (Field.ParentNode is TInfoNode) or
      { interpolators }
      (Field.ParentNode is TAbstractInterpolatorNode) or
      (Field.ParentNode is TNurbsOrientationInterpolatorNode) or
      (Field.ParentNode is TNurbsPositionInterpolatorNode_3) or
      (Field.ParentNode is TNurbsSurfaceInterpolatorNode) or
      (Field.ParentNode is TNurbsPositionInterpolatorNode_2) or
      { Just like sensors, scripts don't affect actual content directly.
        Script nodes take care themselves to react to events send to them. }
      (Field.ParentNode is TAbstractScriptNode) or
      { event utils }
      (Field.ParentNode is TAbstractSequencerNode) or
      (Field.ParentNode is TAbstractTriggerNode) or
      (Field.ParentNode is TBooleanFilterNode) or
      (Field.ParentNode is TBooleanToggleNode) or
      (Field.ParentNode is TTogglerNode) or
      (Field.ParentNode is TLoggerNode) or
      { A change to a prototype field has no real effect,
        TX3DPrototypeNode will only pass it forward to the actual node }
      (Field.ParentNode is TX3DPrototypeNode) or
      { no need to do anything }
      FieldIs(Field, TAbstractTimeDependentNode, 'loop') or
      FieldIs(Field, TMovieTextureNode, 'loop') or
      FieldIs(Field, TAbstractX3DViewpointNode, 'description') or
      FieldIs(Field, TRenderedTextureNode, 'description') or
      FieldIs(Field, TMovieTextureNode, 'description') or
      FieldIs(Field, TAbstractX3DViewpointNode, 'jump') or { also not implemented }
      FieldIs(Field, TAbstractX3DViewpointNode, 'retainUserOffsets') or { also not implemented }
      FieldIs(Field, TAbstractX3DViewpointNode, 'centerOfRotation') or { also not implemented }
      FieldIs(Field, TAbstractViewpointNode, 'cameraMatrixSendAlsoOnOffscreenRendering') or
      FieldIs(Field, TAbstractCameraNode_1, 'focalDistance') or
      FieldIs(Field, TPerspectiveCameraNode, 'heightAngle') or
      FieldIs(Field, TOrthographicCameraNode, 'height') or
      FieldIs(Field, TMovieTextureNode, 'speed') or
      FieldIs(Field, TSeparatorNode, 'renderCulling') or { ignored }
      FieldIs(Field, TInlineNode, 'load') or { handled by eventout callback }
      FieldIs(Field, TInlineNode, 'url') or { handled by eventout callback }
      FieldIs(Field, TAnchorNode, 'parameter') or
      FieldIs(Field, TAnchorNode, 'url') or
      FieldIs(Field, TAnchorNode, 'description') or
      { H-Anim nodes. There are a lot of fields we ignore,
        because we're only interested in animating H-Anim models,
        not editing them (or using with physics). }
      (Field.ParentNode is THAnimDisplacerNode) or
      (Field.ParentNode is THAnimHumanoidNode) or
      (Field.ParentNode is THAnimJointNode) or
      (Field.ParentNode is THAnimSegmentNode) or
      (Field.ParentNode is THAnimSiteNode) or
      (Field.ParentNode is TDisplacerNode) or
      (Field.ParentNode is THumanoidNode) or
      (Field.ParentNode is TJointNode) or
      (Field.ParentNode is TSegmentNode) or
      (Field.ParentNode is TSiteNode) or
      { "update" field of generated textures --- this actually has
        Changes <> [] when needed }
      FieldIs(Field, TGeneratedShadowMapNode, 'update') or
      FieldIs(Field, TRenderedTextureNode, 'update') or
      FieldIs(Field, TGeneratedCubeMapTextureNode, 'update') or
      { My own spec doesn't specify what happens when these change.
        We can just ignore it? }
      FieldIs(Field, TKambiInlineNode, 'replaceNames') or
      FieldIs(Field, TKambiInlineNode, 'replaceNodes') or
      { TODO: stuff implemented, but changes not implemented
        (not even chEverything would help) }
      (Field.ParentNode is TNavigationInfoNode) or
      { TODO: stuff not implemented / things we don't look at all }
      FieldIs(Field, TAbstractLightNode, 'showProxyGeometry') or
      FieldIs(Field, TRenderedTextureNode, 'triggerName') or
      (Field.ParentNode is TLODNode_1) or
      (Field.Name = 'bboxSize') or
      (Field.Name = 'bboxCenter') or
      (Field.ParentNode is TTextureBackgroundNode) or
      (Field.ParentNode is TGeoCoordinateNode) or
      (Field.ParentNode is TGeoLocationNode) or
      (Field.ParentNode is TGeoLODNode) or
      (Field.ParentNode is TGeoMetadataNode) or
      (Field.ParentNode is TGeoOriginNode) or
      (Field.ParentNode is TGeoTransformNode) or
      (Field.ParentNode is TGeoViewpointNode) or
      (Field.ParentNode is TAbstractNBodyCollidableNode) or
      (Field.ParentNode is TAbstractNBodyCollisionSpaceNode) or
      (Field.ParentNode is TAbstractRigidJointNode) or
      (Field.ParentNode is TAbstractPickSensorNode) or
      (Field.ParentNode is TAbstractFollowerNode) or
      (Field.ParentNode is TAbstractParticleEmitterNode) or
      (Field.ParentNode is TAbstractParticlePhysicsModelNode) or
      (Field.ParentNode is TDisplacerNode) or
      (Field.ParentNode is TCoordinateDeformerNode) or
      (Field.ParentNode is TNurbsGroupNode) or
      (Field.ParentNode is TAudioClipNode) or
      (Field.ParentNode is TSoundNode) or
      (Field.ParentNode is TEaseInEaseOutNode) or
      (Field.ParentNode is TFogCoordinateNode) or
      (Field.ParentNode is TLocalFogNode) or
      (Field.ParentNode is TEspduTransformNode) or
      (Field.ParentNode is TPackagedShaderNode) or
      (Field.ParentNode is TProgramShaderNode) or
      (Field.ParentNode is TLayerNode) or
      (Field.ParentNode is TLayerSetNode) or
      (Field.ParentNode is TLayoutNode) or
      (Field.ParentNode is TLayoutLayerNode) or
      (Field.ParentNode is TLayoutGroupNode) or
      (Field.ParentNode is TDISEntityTypeMappingNode) or
      (Field.ParentNode is TDISEntityManagerNode) or
      (Field.ParentNode is TFillPropertiesNode) or
      (Field.ParentNode is TLinePropertiesNode) or
      (Field.ParentNode is TConverterNode) or
      (Field.ParentNode is TScreenFontStyleNode) or
      (Field.ParentNode is TScreenGroupNode) or
      (Field.ParentNode is TCollisionCollectionNode) or
      (Field.ParentNode is TContactNode) or
      (Field.ParentNode is TRigidBodyNode) or
      (Field.ParentNode is TRigidBodyCollectionNode) or
      (Field.ParentNode is TPickableGroupNode) or
      (Field.ParentNode is TParticleSystemNode) or
      (Field.ParentNode is TViewpointGroupNode) or
      (Field.ParentNode is TViewportNode) or
      (Field.ParentNode is TShaderProgramNode) or
      (Field.ParentNode is TAbstractVertexAttributeNode) or
      FieldIs(Field, TAbstractX3DGroupingNode, 'render') or { "render" fields, extensions from InstantReality }
      FieldIs(Field, TBillboardNode, 'axisOfRotation') or
      FieldIs(Field, TAbstractLODNode, 'forceTransitions') or
      (Field.ParentNode is TCADAssemblyNode) or
      (Field.ParentNode is TCADFaceNode) or
      (Field.ParentNode is TCADLayerNode) or
      (Field.ParentNode is TCADPartNode) or
      (Field.ParentNode is TNurbsTextureCoordinateNode) or
      (Field.ParentNode is TNurbsSetNode) or
      (Field.ParentNode is TNurbsCurve2DNode) or
      (Field.ParentNode is TContourPolyline2DNode) or
      (Field.ParentNode is TNurbsTextureSurfaceNode) or
      FieldIs(Field, TScreenEffectNode, 'needsDepth') or
      (Field.ParentNode is TLayer2DNode) or
      (Field.ParentNode is TLayer3DNode) or
      (Field.ParentNode is TKambiHeadLightNode) or
      false { just to have nice newlines };
  end;

var
  I, J: Integer;
  N: TX3DNode;
  Changes: TVRMLChanges;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create('', '');
    try
      for J := 0 to N.Fields.Count - 1 do
      try
        Changes := N.Fields[J].Changes;
        Assert((Changes <> []) or ConfirmedEmptyChanges(N.Fields[J]));
      except
        Writeln('Empty TVRMLField.Changes unconfirmed on ', N.ClassName, '.', N.Fields[J].Name);
        raise;
      end;
    finally FreeAndNil(N) end;
  end;
end;

procedure TTestVRMLNodes.TestTimeDependentNodeHandlerAvailable;

  procedure CheckTimeDependentNodeHandler(N: TX3DNode);
  var
    B: boolean;
    C: TKamTime;
  begin
    { CheckTimeDependentNodeHandler is a separate procedure,
      to limit lifetime of temporary IAbstractTimeDependentNode,
      see "Reference counting" notes on
      http://freepascal.org/docs-html/ref/refse40.html }
    if Supports(N, IAbstractTimeDependentNode) then
    begin
      B := (N as IAbstractTimeDependentNode).TimeDependentNodeHandler.IsActive;
      C := (N as IAbstractTimeDependentNode).TimeDependentNodeHandler.CycleInterval;
    end else
    if (N is TMovieTextureNode) or
       (N is TAudioClipNode) or
       (N is TTimeSensorNode) then
      Assert(false, 'Node ' + N.ClassName + ' should support IAbstractTimeDependentNode');
  end;

var
  I: Integer;
  N: TX3DNode;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create('', '');
    try
      CheckTimeDependentNodeHandler(N);
    except
      Writeln('TestTimeDependentNodeHandlerAvailable failed for ', N.ClassName);
      raise;
    end;
    FreeAndNil(N);
  end;
end;

procedure TTestVRMLNodes.TestITransformNode;

  function ContainsCHTransformField(const N: TX3DNode): boolean;
  var
    I: Integer;
  begin
    for I := 0 to N.Fields.Count - 1 do
      if chTransform in N.Fields[I].Changes then
        Exit(true);
    Result := false;
  end;

var
  I: Integer;
  N: TX3DNode;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create('', '');
    try
      { if a node has field with chTransform, it must support ITransformNode.
        TVRMLScene.HandleChangeTransform assumes this. }
      if ContainsCHTransformField(N) then
        Assert(Supports(N, ITransformNode));

      { if, and only if, a node supports ITransformNode, it must have
        TransformationChange = ntcTransform }
      Assert(
        Supports(N, ITransformNode) =
        (N.TransformationChange = ntcTransform));
    except
      Writeln('TestITransformNode failed for ', N.ClassName);
      raise;
    end;
    FreeAndNil(N);
  end;
end;

procedure TTestVRMLNodes.TestSortPositionInParent;
var
  List: TVRMLFileItemList;
  I0, I1, I2, I3, I4, I5: TTimeSensorNode;
begin
  I0 := nil;
  I1 := nil;
  I2 := nil;
  I3 := nil;
  I4 := nil;
  I5 := nil;
  List := nil;
  try
    I0 := TTimeSensorNode.Create('', '');
    I1 := TTimeSensorNode.Create('', '');
    I2 := TTimeSensorNode.Create('', '');
    I3 := TTimeSensorNode.Create('', '');
    I4 := TTimeSensorNode.Create('', '');
    I4.PositionInParent := -10;
    I5 := TTimeSensorNode.Create('', '');
    I5.PositionInParent := 10;
    List := TVRMLFileItemList.Create(false);

    { QuickSort, used underneath SortPositionInParent, is not stable.
      Which means that items with equal PositionInParent (default -1
      for all) could get mixed, which could break e.g. saving VRML/X3D
      generated by X3DLoadInternal3DS.

      This is avoided by internal PositionOnList. Here we test that it works. }

    List.Add(I0);
    List.Add(I1);
    List.Add(I2);
    List.Add(I3);
    List.Add(I4);
    List.Add(I5);
    List.SortPositionInParent;
    Assert(List[0] = I4);
    Assert(List[1] = I0);
    Assert(List[2] = I1);
    Assert(List[3] = I2);
    Assert(List[4] = I3);
    Assert(List[5] = I5);

    List.Clear;

    List.Add(I2);
    List.Add(I0);
    List.Add(I3);
    List.Add(I1);
    List.Add(I4);
    List.Add(I5);
    List.SortPositionInParent;
    Assert(List[0] = I4);
    Assert(List[1] = I2);
    Assert(List[2] = I0);
    Assert(List[3] = I3);
    Assert(List[4] = I1);
    Assert(List[5] = I5);
  finally
    FreeAndNil(I0);
    FreeAndNil(I1);
    FreeAndNil(I2);
    FreeAndNil(I3);
    FreeAndNil(I4);
    FreeAndNil(I5);
    FreeAndNil(List);
  end;
end;

function LoadVRMLClassicStream(Stream: TStream): TX3DRootNode;
var
  BS: TBufferedReadStream;
begin
  BS := TBufferedReadStream.Create(Stream, false);
  try
    Result := LoadVRMLClassic(BS , '');
  finally FreeAndNil(BS) end;
end;

procedure TTestVRMLNodes.TestRootNodeMeta;
{ Test reading, writing, copying of TX3DRootNode profile, component, metas.
  Also, test updating metas when saving, with generator and source. }
var
  Node, NewNode: TX3DRootNode;
  TempStream: TMemoryStream;
begin
  TempStream := nil;
  Node := nil;

  try
    TempStream := TMemoryStream.Create;

    Node := LoadVRMLClassicFromString('#X3D V3.1 utf8' +NL+
      'PROFILE Immersive' +NL+
      'COMPONENT NURBS:2' +NL+
      'COMPONENT Shaders:1' +NL+
      'META "test''''key" "test\"value"' +NL+
      'META "generator" "testgenerator and & weird '' chars \" test"', '');

    { make sure loaded from string Ok }
    Assert(Node.HasForceVersion);
    Assert(Node.ForceVersion.Major = 3);
    Assert(Node.ForceVersion.Minor = 1);
    Assert(Node.Profile = 'Immersive');
    Assert(Node.Components.Count = 2);
    Assert(Node.Components['NURBS'] = 2);
    Assert(Node.Components['Shaders'] = 1);
    Assert(Node.Meta.Count = 2);
    Assert(Node.Meta['test''''key'] = 'test"value');
    Assert(Node.Meta['generator'] = 'testgenerator and & weird '' chars " test');

    { save and load again }
    SaveVRML(Node, TempStream, '', '', xeClassic, false);
    FreeAndNil(Node);
    TempStream.Position := 0;
    Node := LoadVRMLClassicStream(TempStream);

    { make sure saved and loaded back Ok }
    Assert(Node.HasForceVersion);
    Assert(Node.ForceVersion.Major = 3);
    Assert(Node.ForceVersion.Minor = 1);
    Assert(Node.Profile = 'Immersive');
    Assert(Node.Components.Count = 2);
    Assert(Node.Components['NURBS'] = 2);
    Assert(Node.Components['Shaders'] = 1);
    Assert(Node.Meta.Count = 2);
    Assert(Node.Meta['test''''key'] = 'test"value');
    Assert(Node.Meta['generator'] = 'testgenerator and & weird '' chars " test');

    { tweak some Meta }
    Node.Meta['test''''key'] := 'newvalue';
    Node.Meta['testkey2'] := 'newvalue2';

    { replace Node with DeepCopy of itself (should preserve everything) }
    NewNode := Node.DeepCopy as TX3DRootNode;
    FreeAndNil(Node);
    Node := NewNode;
    NewNode := nil;

    { tweak some Meta more }
    Node.Meta['testkey2'] := 'evennewervalue2';
    Node.Meta['testkey3'] := 'newvalue3';

    { save and load again. During SaveVRML tweak meta generator and source }
    TempStream.Position := 0;
    SaveVRML(Node, TempStream, 'newgenerator', 'newsource', xeClassic, false);
    FreeAndNil(Node);
    TempStream.Position := 0;
    Node := LoadVRMLClassicStream(TempStream);

    { make sure saved and loaded back Ok }
    Assert(Node.HasForceVersion);
    Assert(Node.ForceVersion.Major = 3);
    Assert(Node.ForceVersion.Minor = 1);
    Assert(Node.Profile = 'Immersive');
    Assert(Node.Components.Count = 2);
    Assert(Node.Components['NURBS'] = 2);
    Assert(Node.Components['Shaders'] = 1);
    Assert(Node.Meta.Count = 6);
    Assert(Node.Meta['test''''key'] = 'newvalue');
    Assert(Node.Meta['testkey2'] = 'evennewervalue2');
    Assert(Node.Meta['testkey3'] = 'newvalue3');
    Assert(Node.Meta['generator'] = 'newgenerator');
    Assert(Node.Meta['generator-previous'] = 'testgenerator and & weird '' chars " test');
    Assert(Node.Meta['source'] = 'newsource');

    { save and load again, this time going though XML }
    TempStream.Position := 0;
    SaveVRML(Node, TempStream, '', '', xeXML, false);
    FreeAndNil(Node);
    TempStream.Position := 0;
    Node := LoadX3DXml(TempStream, '');

    { make sure saved and loaded back Ok }
    Assert(Node.HasForceVersion);
    Assert(Node.ForceVersion.Major = 3);
    Assert(Node.ForceVersion.Minor = 1);
    Assert(Node.Profile = 'Immersive');
    Assert(Node.Components.Count = 2);
    Assert(Node.Components['NURBS'] = 2);
    Assert(Node.Components['Shaders'] = 1);
    Assert(Node.Meta.Count = 6);
    Assert(Node.Meta['test''''key'] = 'newvalue');
    Assert(Node.Meta['testkey2'] = 'evennewervalue2');
    Assert(Node.Meta['testkey3'] = 'newvalue3');
    Assert(Node.Meta['generator'] = 'newgenerator');
    Assert(Node.Meta['generator-previous'] = 'testgenerator and & weird '' chars " test');
    Assert(Node.Meta['source'] = 'newsource');

  finally
    FreeAndNil(Node);
    FreeAndNil(TempStream);
  end;
end;

procedure TTestVRMLNodes.TestConvertToX3D;
var
  Node: TX3DRootNode;
  TempStream: TMemoryStream;
begin
  TempStream := nil;
  Node := nil;

  try
    TempStream := TMemoryStream.Create;

    { load X3D 3.1 }
    Node := LoadVRMLClassicFromString('#X3D V3.1 utf8' +NL+
      'PROFILE Immersive', '');
    Assert(Node.HasForceVersion = true);
    Assert(Node.ForceVersion.Major = 3);
    Assert(Node.ForceVersion.Minor = 1);

    { save to XML }
    TempStream.Position := 0;
    TempStream.Size := 0;
    SaveVRML(Node, TempStream, '', '', xeXML, true);
    FreeAndNil(Node);

    { check that loading it back results in 3.1 }
    TempStream.Position := 0;
    Node := LoadX3DXml(TempStream, '');
    Assert(Node.HasForceVersion = true);
    Assert(Node.ForceVersion.Major = 3);
    Assert(Node.ForceVersion.Minor = 1);

    { save to clasic }
    TempStream.Position := 0;
    TempStream.Size := 0;
    SaveVRML(Node, TempStream, '', '', xeClassic, true);
    FreeAndNil(Node);

    { check that loading it back results in 3.1 }
    TempStream.Position := 0;
    Node := LoadVRMLClassicStream(TempStream);
    Assert(Node.HasForceVersion = true);
    Assert(Node.ForceVersion.Major = 3);
    Assert(Node.ForceVersion.Minor = 1);
    FreeAndNil(Node);

    { load VRML 2.0 }
    Node := LoadVRMLClassicFromString('#VRML V2.0 utf8' + NL, '');
    Assert(Node.HasForceVersion = true);
    Assert(Node.ForceVersion.Major = 2);
    Assert(Node.ForceVersion.Minor = 0);

    { save to XML }
    TempStream.Position := 0;
    TempStream.Size := 0;
    SaveVRML(Node, TempStream, '', '', xeXML, false);
    FreeAndNil(Node);

    { check that loading it back results in 3.0
      (convertion was done, since this is XML) }
    TempStream.Position := 0;
    Node := LoadX3DXml(TempStream, '');
    Assert(Node.HasForceVersion = true);
    Assert(Node.ForceVersion.Major = 3);
    Assert(Node.ForceVersion.Minor = 0);
    FreeAndNil(Node);

    { load VRML 2.0 }
    Node := LoadVRMLClassicFromString('#VRML V2.0 utf8' + NL, '');
    Assert(Node.HasForceVersion = true);
    Assert(Node.ForceVersion.Major = 2);
    Assert(Node.ForceVersion.Minor = 0);

    { save to classic }
    TempStream.Position := 0;
    TempStream.Size := 0;
    SaveVRML(Node, TempStream, '', '', xeClassic, false);
    FreeAndNil(Node);

    { check that loading it back results in 2.0
      (convertion not done, since this is classic and convertion not forced) }
    TempStream.Position := 0;
    Node := LoadVRMLClassicStream(TempStream);
    Assert(Node.HasForceVersion = true);
    Assert(Node.ForceVersion.Major = 2);
    Assert(Node.ForceVersion.Minor = 0);
    FreeAndNil(Node);

    { load VRML 2.0 }
    Node := LoadVRMLClassicFromString('#VRML V2.0 utf8' + NL, '');
    Assert(Node.HasForceVersion = true);
    Assert(Node.ForceVersion.Major = 2);
    Assert(Node.ForceVersion.Minor = 0);

    { save to classic }
    TempStream.Position := 0;
    TempStream.Size := 0;
    SaveVRML(Node, TempStream, '', '', xeClassic, true);
    FreeAndNil(Node);

    { check that loading it back results in 3.0
      (convertion done, since forced = true) }
    TempStream.Position := 0;
    Node := LoadVRMLClassicStream(TempStream);
    Assert(Node.HasForceVersion = true);
    Assert(Node.ForceVersion.Major = 3);
    Assert(Node.ForceVersion.Minor = 0);
    FreeAndNil(Node);
  finally
    FreeAndNil(Node);
    FreeAndNil(TempStream);
  end;
end;

procedure TTestVRMLNodes.TestReadingWritingQuotes;
const
  ValidString = 'test string with " and '' and \ and / inside';
  ValidString2 = '" another '''' test string  with some weirdness \\ inside';

  function CreateTestScene: TX3DRootNode;
  var
    Text: TTextNode;
    Shape: TShapeNode;
    Touch: TTouchSensorNode;
  begin
    Text := TTextNode.Create('', '');
    Text.FdString.Items.Add(ValidString);
    Text.FdString.Items.Add(ValidString2);

    Shape := TShapeNode.Create('', '');
    Shape.FdGeometry.Value := Text;

    Touch := TTouchSensorNode.Create('', '');
    Touch.FdDescription.Value := ValidString;

    Result := TX3DRootNode.Create('', '');
    Result.FdChildren.Add(Shape);
    Result.FdChildren.Add(Touch);
  end;

  procedure Assertions(Node: TX3DRootNode);
  var
    StringField: TMFString;
  begin
    StringField := ((Node.FdChildren[0] as TShapeNode).FdGeometry.Value as TTextNode).FdString;
    Assert(StringField.Count = 2);
    Assert(StringField.Items[0] = ValidString);
    Assert(StringField.Items[1] = ValidString2);
    Assert((Node.FdChildren[1] as TTouchSensorNode).FdDescription.Value = ValidString);
  end;

var
  Node: TX3DRootNode;
  TempStream: TMemoryStream;
begin
  TempStream := nil;
  Node := nil;

  try
    Node := CreateTestScene;
    Assertions(Node);

    TempStream := TMemoryStream.Create;

    TempStream.Position := 0;
    TempStream.Size := 0;
    SaveVRML(Node, TempStream, '', '', xeClassic, true);
    FreeAndNil(Node);

    TempStream.Position := 0;
    Node := LoadVRMLClassicStream(TempStream);
    Assertions(Node);

    TempStream.Position := 0;
    TempStream.Size := 0;
    SaveVRML(Node, TempStream, '', '', xeXML, true);
    FreeAndNil(Node);

    TempStream.Position := 0;
    Node := LoadX3DXml(TempStream, '');
    Assertions(Node);
  finally
    FreeAndNil(Node);
    FreeAndNil(TempStream);
  end;
end;

initialization
 RegisterTest(TTestVRMLNodes);
end.
