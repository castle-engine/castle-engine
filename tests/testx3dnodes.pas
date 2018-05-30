{
  Copyright 2004-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestX3DNodes;

{$I tests.inc}
{$I castleconf.inc}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, CastleBaseTestCase,
  CastleVectors, X3DNodes;

type
  TTestX3DNodes = class(TCastleBaseTestCase)
  private
    procedure WeakLinkUnusedWarning(Sender: TObject; const Category, S: string);
  published
    procedure TestNodesManager;

    { This is really large test that reads and writes various VRML files
      and checks whether the generated VRML file is the same.
      It checks "the same" by comparing sequence of X3D lexer
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

    { Almost all VRML 1 state nodes should have Changes = [chVisibleVRML1State]
      (and possibly more, like chUseBlending). }
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
    { Later: maintaining a list of exceptions to this test was not efficient.
      And by default we generate non-empty ChangesAlways, so it's not easy
      to make this mistake anymore. }
    // procedure TestEmptyChanges;

    { Try calling GetInternalTimeDependentHandler
      on every IAbstractTimeDependentNode, and use the handler.
      Catches e.g. not overriden CycleInterval. }
    procedure TestInternalTimeDependentHandlerAvailable;

    procedure TestITransformNode;
    procedure TestSortPositionInParent;
    procedure TestRootNodeMeta;
    procedure TestConvertToX3D;
    procedure TestReadingWritingQuotes;
    procedure TestSolid;
    procedure TestConvex;
    procedure TestX3DXmlString;
    procedure TestOrthoViewpointFieldOfView;
    procedure TestFontStyle;
    procedure TestWeakLinkUnusedWarning;
    procedure TestKeepExisting;
    procedure TestAttenuation;
    procedure TestAddChildren;
    procedure TestNurbsCurvePoint;
  end;

implementation

uses Generics.Collections,
  CastleUtils, CastleInternalX3DLexer, CastleClassUtils, CastleFilesUtils,
  X3DFields, CastleTimeUtils, CastleDownload, X3DLoad, X3DTime,
  CastleApplicationProperties, CastleTextureImages;

{ TNode* ------------------------------------------------------------ }

type
  TSpecialNode = class(TX3DNode)
    function X3DType: string; override;
  end;

function TSpecialNode.X3DType: string;
begin
 result := 'OohImSoSpecial';
end;

type
  TSomethingNode = class(TX3DNode)
    class function ClassX3DType: string; override;
  end;

class function TSomethingNode.ClassX3DType: string;
begin
 result := 'WellImNothingSpecial';
end;

{ ------------------------------------------------------------ }

procedure TTestX3DNodes.TestNodesManager;
begin
 try
  { throw exception because TSpecialNode.ClassX3DType = '' }
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

{ TX3DTokenInfo and TX3DTokenInfoList ---------------------------------- }

type
  TX3DTokenInfo = class
    Token: TX3DToken;
    Float: Float; //< for both vtFloat and vtInteger
    Name: string; //< for vtName
    AString: string; //< for vtString
    Keyword: TX3DKeyword; //< for vtKeyword
    Integer: Int64; //< for vtInteger
  end;

  TX3DTokenInfoList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TX3DTokenInfo>)
    procedure AssertEqual(const TestCase: TTestCase; SecondValue: TX3DTokenInfoList);
    procedure ReadFromFile(const FileName: string);
  end;

procedure TX3DTokenInfoList.AssertEqual(const TestCase: TTestCase;
  SecondValue: TX3DTokenInfoList);

  procedure AssertEqualTokens(const T1, T2: TX3DTokenInfo);

    function DescribeTokenInfo(const T: TX3DTokenInfo): string;
    const
      VRMLTokenNames: array[TX3DToken]of string = (
        'keyword', 'name',
        '"{"', '"}"', '"["', '"]"', '"("', '")"', '"|"', '","', '"."', '":"',
        'float', 'integer', 'string', 'end of stream');
    begin
      Result := VRMLTokenNames[T.Token];
      case T.Token of
        vtKeyword: result := result +' "' +X3DKeywordsName[T.Keyword]+'"';
        vtName: result := '"' +T.Name+'"';
        vtFloat: result := result +' ' +FloatToStr(T.Float);
        vtInteger: result := result +' ' +IntToStr(T.Integer);
        vtString: result := result+' "'+T.AString+'"';
      end;
    end;

  begin
    if not
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
      ( (T1.Token <> vtString) or (T1.AString = T2.AString) ) then
      TestCase.Fail(Format('VRML tokens different: %s and %s',
        [DescribeTokenInfo(T1), DescribeTokenInfo(T2)]));
  end;

var
  I: Integer;
begin
  TestCase.AssertEquals(Count, SecondValue.Count);
  for I := 0 to Count - 1 do
    AssertEqualTokens(Items[I], SecondValue[I]);
end;

{ Note that this can be used to test correctly only files that can
  be correctly parsed by pure Lexer.NextToken calls. All valid VRML >= 2.0
  files are like that, although parser in practice has to use NextTokenForceXxx
  methods because of unfortunately
  1. invalid VRML files (that use some funny node names)
  2. VRML 1.0 ugly feature that string doesn't have to be enclosed in "" }
procedure TX3DTokenInfoList.ReadFromFile(const FileName: string);
var
  Lexer: TX3DLexer;

  function CurrentToken: TX3DTokenInfo;
  begin
    Result := TX3DTokenInfo.Create;
    Result.Token := Lexer.Token;
    Result.Keyword := Lexer.TokenKeyword;
    Result.Name := Lexer.TokenName;
    Result.Float := Lexer.TokenFloat;
    Result.Integer := Lexer.TokenInteger;
    Result.AString := Lexer.TokenString;
  end;

  function LexerFromFile(const URL: string): TX3DLexer;
  var
    Stream: TStream;
  begin
    Stream := Download(URL);
    Result := TX3DLexer.Create(TBufferedReadStream.Create(Stream, true), true);
  end;

begin
  Count := 0;
  Lexer := LexerFromFile(FileName);
  try
    Add(CurrentToken);
    while Lexer.Token <> vtEnd do
    begin
      Lexer.NextToken;
      Add(CurrentToken);
    end;
  finally FreeAndNil(Lexer); end;
end;

procedure TTestX3DNodes.TestParseSaveToFile;

  procedure TestReadWrite(const FileName: string);
  var
    First, Second: TX3DTokenInfoList;
    Node: TX3DNode;
    NewFile: string;
  begin
    First := nil;
    Second := nil;
    Node := nil;
    try
      First := TX3DTokenInfoList.Create;
      First.ReadFromFile(FileName);

      Node := LoadX3DClassic(FileName, false, false);
      NewFile := InclPathDelim(GetTempDir) + 'test_castle_game_engine.x3dv';
      Save3D(Node, NewFile, ApplicationName, '', xeClassic, false);

      Second := TX3DTokenInfoList.Create;
      Second.ReadFromFile(NewFile);

      First.AssertEqual(Self, Second);
    finally
      FreeAndNil(First);
      FreeAndNil(Second);
      FreeAndNil(Node);
    end;
  end;

begin
  // TODO: This will never pass for now, because writing adds 3 new tokens:
  // META "generator" "test_castle_game_engine"
  // so the Second file has always 3 more tokens.

  // TestReadWrite('data/demo-models-copy/proto_sfnode_default.x3dv');
  // TestReadWrite('data/demo-models-copy/tricky_def_use.x3dv');
end;

procedure TTestX3DNodes.TestInterfaceSupports;
var
  L: TX3DNodeClassesList;

  function IndexOfAnyAncestorByClass(C: TX3DNodeClass): boolean;
  var
    N: TX3DNode;
  begin
    N := C.Create;
    try
      Result := L.IndexOfAnyAncestor(N) <> -1;
    finally FreeAndNil(N) end;
  end;

begin
  { When our interfaces have appropriate GUIDs, "Supports" works Ok. }

  AssertTrue(Supports(TGroupNode_2, IAbstractChildNode));
  AssertTrue(Supports(TSwitchNode_2, IAbstractChildNode));
  AssertTrue(not Supports(TConeNode_2, IAbstractChildNode));
  AssertTrue(not Supports(TAppearanceNode, IAbstractChildNode));
  AssertTrue(not Supports(TX3DNode, IAbstractChildNode));
  AssertTrue(not Supports(TObject, IAbstractChildNode));

  L := TX3DNodeClassesList.Create;
  try
    L.AddRegisteredImplementing(IAbstractChildNode);
    { similar to above tests, but now using L.IndexOfAnyAncestor.
      So we test IndexOfAnyAncestor and AddRegisteredImplementing,
      AddRegisteredImplementing also uses "Supports" under the hood
      and results should be the same. }
    AssertTrue(IndexOfAnyAncestorByClass(TGroupNode_2));
    AssertTrue(IndexOfAnyAncestorByClass(TSwitchNode_2));
    AssertTrue(not IndexOfAnyAncestorByClass(TConeNode_2));
    AssertTrue(not IndexOfAnyAncestorByClass(TAppearanceNode));
    AssertTrue(not IndexOfAnyAncestorByClass(TX3DNode));
  finally FreeAndNil(L) end;
end;

procedure TTestX3DNodes.TestUniqueFields;
var
  I, J, K: Integer;
  N: TX3DNode;
  CurrentName: string;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create;
    try

      { Writeln(N.X3DType, ' ', Supports(N, IAbstractChildNode)); }

      { Test that all fields, events names are different.

        Doesn't detect if two alternative names match each other for now!
        (Although will detect if some alternative name will match non-alternative
        name, since uses IsName comparison).

        Also, doesn't check the implicitly exposed events for now. }

      for J := 0 to N.FieldsCount - 1 do
      begin
        CurrentName := N.Fields[J].X3DName;
        for K := 0 to N.FieldsCount - 1 do
          AssertTrue((K = J) or (not N.Fields[K].IsName(CurrentName)));
        for K := 0 to N.EventsCount - 1 do
          AssertTrue(not N.Events[K].IsName(CurrentName));
      end;

      for J := 0 to N.EventsCount - 1 do
      begin
        CurrentName := N.Events[J].X3DName;
        for K := 0 to N.FieldsCount - 1 do
          AssertTrue(not N.Fields[K].IsName(CurrentName));
        for K := 0 to N.EventsCount - 1 do
          AssertTrue((K = J) or (not N.Events[K].IsName(CurrentName)));
      end;
    finally FreeAndNil(N) end;
  end;
end;

procedure TTestX3DNodes.TestAllowedChildren;
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

    They were removed from X3DNodes, since using X3D inheritance
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
    TCoordinate3Node_1, TFontStyleNode_1, TInfoNode_1, TLODNode_1, TMaterialNode_1,

    { TNormalNode used to also be allowed here, but it's also used by X3D,
      and I don't want to mess X3D inheritance by making TNormalNode descendant
      of IAbstractChildNode --- which is required only when you mix VRML 1.0
      and X3D... When mixing VRML 1.0 and VRML >= 2.0 in a singe file,
      you will have to live with warnings about Normal not allowed as
      children in VRML >= 2.0 nodes.

      Also TTexture2Node_1 was here, but is removed: I don't want to mess
      X3D inheritance just for VRML 1.0 + 2.0 mixing feature. }

    TMaterialBindingNode_1, TNormalBindingNode_1,
    TTexture2TransformNode_1,
    TTextureCoordinate2Node_1, TShapeHintsNode_1,
    TMatrixTransformNode_1, TRotationNode_1,
    TScaleNode_1, TTransformNode_1,
    TTranslationNode_1,
    TOrthographicCameraNode_1, TPerspectiveCameraNode_1,
    TDirectionalLightNode_1, TPointLightNode_1, TSpotLightNode_1,
    TGroupNode_1, TSeparatorNode_1, TSwitchNode_1, TTransformSeparatorNode_1,
    TWWWAnchorNode_1,
    TWWWInlineNode_1,

    { Kambi non-standard nodes }
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
    TContour2DNode_2,
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
      AssertTrue(Supports(AllowedChildrenNodes[I], IAbstractChildNode));

      { Just to make sure, check also the created class
        (I don't trust FPC interfaces for now...) }
      N := AllowedChildrenNodes[I].Create;
      try
        AssertTrue(Supports(N, IAbstractChildNode));
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
      AssertTrue(AllowedGeometryNodes[I].InheritsFrom(TAbstractGeometryNode));
    except
      on E: Exception do
      begin
        Writeln('Failed on ', AllowedGeometryNodes[I].ClassName, ' is TAbstractGeometryNode');
        raise;
      end;
    end;

  finally
    FreeAndNil(AllowedGeometryNodes);
    FreeAndNil(AllowedChildrenNodes);
  end;
end;

procedure TTestX3DNodes.TestContainerFieldList;
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
    'NurbsCurve2D=children' + NL +
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
    AssertTrue(ContainerFieldList.IndexOfName('WorldInfo') <> -1);
    AssertTrue(ContainerFieldList.IndexOfName('Anchor') <> -1);
    AssertTrue(ContainerFieldList.IndexOfName('NotExisting') = -1);

    for I := 0 to NodesManager.RegisteredCount - 1 do
    begin
      N := NodesManager.Registered[I].Create;
      try
        Index := ContainerFieldList.IndexOfName(N.X3DType);
        if (Index <> -1) and
           (not (N is TFontStyleNode_1)) and
           (not (N is TMaterialNode_1)) then
        try
          AssertTrue(ContainerFieldList.ValueFromIndex[Index] = N.DefaultContainerField);
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

procedure TTestX3DNodes.TestContainerFieldGeometry;
var
  I: Integer;
  N: TX3DNode;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create;
    try
      if (N is TAbstractGeometryNode) and
         { TContour2DNode_2 is an exception, it has containerField=trimmingContour.
           This isn't really mandated by any specification,
           as VRML 97 spec doesn't use XML encoding,
           so it doesn't specify containerField. }
         (not (N is TContour2DNode_2)) then
      try
        AssertTrue(N.DefaultContainerField = 'geometry');
      except
        on E: Exception do
        begin
          Writeln('Failed on ', N.ClassName, ': it should have containerField=geometry');
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

procedure TTestX3DNodes.TestDestructionNotification;
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
    AssertTrue(A.IndexOf(@M1.Foo) = 0);
    AssertTrue(A.IndexOf(@M2.Foo) = 1);
    AssertTrue(A.IndexOf(@M3.Foo) = 2);
    A.Remove(@M2.Foo);
    AssertTrue(A.IndexOf(@M1.Foo) = 0);
    AssertTrue(A.IndexOf(@M2.Foo) = -1);
    AssertTrue(A.IndexOf(@M3.Foo) = 1);
  finally
    FreeAndNil(A);
    FreeAndNil(M1);
    FreeAndNil(M2);
    FreeAndNil(M3);
  end;
end;

procedure TTestX3DNodes.TestGeometryNodesImplemented;
var
  I: Integer;
  N: TX3DNode;
  G, ProxyGeometry: TAbstractGeometryNode;
  State, ProxyState: TX3DGraphTraverseState;
begin
  State := TX3DGraphTraverseState.Create;
  try
    for I := 0 to NodesManager.RegisteredCount - 1 do
    begin
      N := NodesManager.Registered[I].Create;
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

procedure TTestX3DNodes.TestGeometryNodesChanges;
var
  I, J: Integer;
  N: TX3DNode;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create;
    try
      if N is TAbstractGeometryNode then
      begin
        for J := 0 to N.FieldsCount - 1 do
          if N.Fields[J].X3DName <> 'metadata' then
          try
            AssertTrue(N.Fields[J].ExecuteChanges = [chGeometry]);
          except
            Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].X3DName);
            raise;
          end;
      end else
      begin
        for J := 0 to N.FieldsCount - 1 do
        try
          AssertTrue(not (chGeometry in N.Fields[J].ExecuteChanges));
        except
          Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].X3DName);
          raise;
        end;
      end

    finally FreeAndNil(N) end;
  end;
end;

procedure TTestX3DNodes.TestVisibleVRML1StateChanges;
var
  I, J: Integer;
  N: TX3DNode;
  VRML1StateNode: TVRML1StateNode;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create;
    try
      if N.VRML1StateNode(VRML1StateNode) and
         (VRML1StateNode <> vsCoordinate3) then
      begin
        for J := 0 to N.FieldsCount - 1 do
          { some fields are allowed exception }
          if (N.Fields[J].X3DName <> 'metadata') and
             (N.Fields[J].X3DName <> 'effects') and
             (N.Fields[J].X3DName <> 'crossOrigin') and
             (N.Fields[J].X3DName <> 'textureProperties') then
          try
            AssertTrue(
              (chVisibleVRML1State in N.Fields[J].ExecuteChanges) or
              (chGeometryVRML1State in N.Fields[J].ExecuteChanges));
          except
            Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].X3DName);
            raise;
          end;
      end else
      begin
        for J := 0 to N.FieldsCount - 1 do
          { some fields are allowed exception }
          if (N.Fields[J].X3DName <> 'alphaChannel') then
          try
            AssertTrue(not (chVisibleVRML1State in N.Fields[J].ExecuteChanges));
            AssertTrue(not (chGeometryVRML1State in N.Fields[J].ExecuteChanges));
          except
            Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].X3DName);
            raise;
          end;
      end

    finally FreeAndNil(N) end;
  end;
end;

procedure TTestX3DNodes.TestColorNodeChanges;
var
  I, J: Integer;
  N: TX3DNode;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create;
    try
      if N is TAbstractColorNode then
      begin
        for J := 0 to N.FieldsCount - 1 do
          if N.Fields[J].X3DName <> 'metadata' then
          try
            AssertTrue(N.Fields[J].ExecuteChanges = [chColorNode]);
          except
            Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].X3DName);
            raise;
          end;
      end else
      begin
        for J := 0 to N.FieldsCount - 1 do
        try
          AssertTrue(not (chColorNode in N.Fields[J].ExecuteChanges));
        except
          Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].X3DName);
          raise;
        end;
      end

    finally FreeAndNil(N) end;
  end;
end;

procedure TTestX3DNodes.TestTextureCoordinate;
var
  I, J: Integer;
  N: TX3DNode;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create;
    try
      if N is TAbstractTextureCoordinateNode then
      begin
        for J := 0 to N.FieldsCount - 1 do
          if N.Fields[J].X3DName <> 'metadata' then
          try
            AssertTrue(N.Fields[J].ExecuteChanges = [chTextureCoordinate]);
          except
            Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].X3DName);
            raise;
          end;
      end else
      begin
        for J := 0 to N.FieldsCount - 1 do
        try
          AssertTrue(not (chTextureCoordinate in N.Fields[J].ExecuteChanges));
        except
          Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].X3DName);
          raise;
        end;
      end

    finally FreeAndNil(N) end;
  end;
end;

(*
procedure TTestX3DNodes.TestEmptyChanges;

  { Confirmed fiels that may have Changes = []. }
  function ConfirmedEmptyChanges(Field: TX3DField): boolean;

    function FieldIs(Field: TX3DField;
      const NodeClass: TX3DNodeClass; const FieldName: string): boolean;
    begin
      Result := (Field.ParentNode is NodeClass) and (Field.X3DName = FieldName);
    end;

  begin
    Result :=
      { Sensors don't affect actual content directly. }
      (Field.ParentNode is TAbstractSensorNode) or
      FieldIs(Field, TTimeSensorNode, 'cycleInterval') or
      FieldIs(Field, TTimeSensorNode, 'enabled') or
      (Field.ParentNode is TWWWAnchorNode_1) or
      { metadata, info nodes }
      FieldIs(Field, TAbstractNode, 'metadata') or
      (Field.ParentNode is TMetadataBooleanNode) or
      (Field.ParentNode is TMetadataDoubleNode) or
      (Field.ParentNode is TMetadataFloatNode) or
      (Field.ParentNode is TMetadataIntegerNode) or
      (Field.ParentNode is TMetadataSetNode) or
      (Field.ParentNode is TMetadataStringNode) or
      (Field.ParentNode is TWorldInfoNode) or
      (Field.ParentNode is TInfoNode_1) or
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
      FieldIs(Field, TAbstractViewpointNode, 'description') or
      FieldIs(Field, TRenderedTextureNode, 'description') or
      FieldIs(Field, TMovieTextureNode, 'description') or
      FieldIs(Field, TAbstractX3DViewpointNode, 'jump') or { also not implemented }
      FieldIs(Field, TAbstractX3DViewpointNode, 'retainUserOffsets') or { also not implemented }
      FieldIs(Field, TAbstractX3DViewpointNode, 'centerOfRotation') or { also not implemented }
      FieldIs(Field, TAbstractViewpointNode, 'cameraMatrixSendAlsoOnOffscreenRendering') or
      FieldIs(Field, TAbstractCameraNode_1, 'focalDistance') or
      FieldIs(Field, TPerspectiveCameraNode_1, 'heightAngle') or
      FieldIs(Field, TOrthographicCameraNode_1, 'height') or
      FieldIs(Field, TMovieTextureNode, 'speed') or
      FieldIs(Field, TSeparatorNode_1, 'renderCulling') or { ignored }
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
      (Field.X3DName = 'bboxSize') or
      (Field.X3DName = 'bboxCenter') or
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
      FieldIs(Field, TGeneratedCubeMapTextureNode, 'bias') or
      FieldIs(Field, TAbstractShapeNode, 'render') or
      false { just to have nice newlines };
  end;

var
  I, J: Integer;
  N: TX3DNode;
  Changes: TX3DChanges;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create;
    try
      for J := 0 to N.FieldsCount - 1 do
      try
        Changes := N.Fields[J].ExecuteChanges;
        AssertTrue((Changes <> []) or ConfirmedEmptyChanges(N.Fields[J]));
      except
        Writeln('Empty TX3DField.Changes unconfirmed on ', N.ClassName, '.', N.Fields[J].X3DName);
        raise;
      end;
    finally FreeAndNil(N) end;
  end;
end;
*)

procedure TTestX3DNodes.TestInternalTimeDependentHandlerAvailable;

  procedure CheckInternalTimeDependentHandler(N: TX3DNode);
  var
    B: boolean;
    C: TFloatTime;
  begin
    { CheckInternalTimeDependentHandler is a separate procedure,
      to limit lifetime of temporary IAbstractTimeDependentNode,
      see "Reference counting" notes on
      http://freepascal.org/docs-html/ref/refse40.html }
    if Supports(N, IAbstractTimeDependentNode) then
    begin
      B := (N as IAbstractTimeDependentNode).InternalTimeDependentHandler.IsActive;
      C := (N as IAbstractTimeDependentNode).InternalTimeDependentHandler.CycleInterval;
    end else
    if (N is TMovieTextureNode) or
       (N is TAudioClipNode) or
       (N is TTimeSensorNode) then
      Fail('Node ' + N.ClassName + ' should support IAbstractTimeDependentNode');
  end;

var
  I: Integer;
  N: TX3DNode;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create;
    try
      CheckInternalTimeDependentHandler(N);
    except
      Writeln('TestInternalTimeDependentHandlerAvailable failed for ', N.ClassName);
      raise;
    end;
    FreeAndNil(N);
  end;
end;

procedure TTestX3DNodes.TestITransformNode;

  function ContainsCHTransformField(const N: TX3DNode): boolean;
  var
    I: Integer;
  begin
    for I := 0 to N.FieldsCount - 1 do
      if chTransform in N.Fields[I].ExecuteChanges then
        Exit(true);
    Result := false;
  end;

var
  I: Integer;
  N: TX3DNode;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create;
    try
      { if a node has field with chTransform, it must support ITransformNode.
        TCastleSceneCore.HandleChangeTransform assumes this. }
      if ContainsCHTransformField(N) then
        AssertTrue(Supports(N, ITransformNode));

      { if, and only if, a node supports ITransformNode, it must have
        TransformationChange = ntcTransform }
      AssertTrue(
        Supports(N, ITransformNode) =
        (N.TransformationChange = ntcTransform));
    except
      Writeln('TestITransformNode failed for ', N.ClassName);
      raise;
    end;
    FreeAndNil(N);
  end;
end;

procedure TTestX3DNodes.TestSortPositionInParent;
var
  List: TX3DFileItemList;
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
    I0 := TTimeSensorNode.Create;
    I1 := TTimeSensorNode.Create;
    I2 := TTimeSensorNode.Create;
    I3 := TTimeSensorNode.Create;
    I4 := TTimeSensorNode.Create;
    I4.PositionInParent := -10;
    I5 := TTimeSensorNode.Create;
    I5.PositionInParent := 10;
    List := TX3DFileItemList.Create(false);

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
    AssertTrue(List[0] = I4);
    AssertTrue(List[1] = I0);
    AssertTrue(List[2] = I1);
    AssertTrue(List[3] = I2);
    AssertTrue(List[4] = I3);
    AssertTrue(List[5] = I5);

    List.Clear;

    List.Add(I2);
    List.Add(I0);
    List.Add(I3);
    List.Add(I1);
    List.Add(I4);
    List.Add(I5);
    List.SortPositionInParent;
    AssertTrue(List[0] = I4);
    AssertTrue(List[1] = I2);
    AssertTrue(List[2] = I0);
    AssertTrue(List[3] = I3);
    AssertTrue(List[4] = I1);
    AssertTrue(List[5] = I5);
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

function LoadX3DClassicStream(Stream: TStream): TX3DRootNode;
var
  BS: TBufferedReadStream;
begin
  BS := TBufferedReadStream.Create(Stream, false);
  try
    Result := LoadX3DClassicInternal(BS , '');
  finally FreeAndNil(BS) end;
end;

procedure TTestX3DNodes.TestRootNodeMeta;
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

    Node := LoadX3DClassicFromString('#X3D V3.1 utf8' +NL+
      'PROFILE Immersive' +NL+
      'COMPONENT NURBS:2' +NL+
      'COMPONENT Shaders:1' +NL+
      'META "test''''key" "test\"value"' +NL+
      'META "generator" "testgenerator and & weird '' chars \" test"', '');

    { make sure loaded from string Ok }
    AssertTrue(Node.HasForceVersion);
    AssertTrue(Node.ForceVersion.Major = 3);
    AssertTrue(Node.ForceVersion.Minor = 1);
    AssertTrue(Node.Profile = 'Immersive');
    AssertTrue(Node.Components.Count = 2);
    AssertTrue(Node.Components['NURBS'] = 2);
    AssertTrue(Node.Components['Shaders'] = 1);
    AssertTrue(Node.Meta.Count = 2);
    AssertTrue(Node.Meta['test''''key'] = 'test"value');
    AssertTrue(Node.Meta['generator'] = 'testgenerator and & weird '' chars " test');

    { save and load again }
    Save3D(Node, TempStream, '', '', xeClassic, false);
    FreeAndNil(Node);
    TempStream.Position := 0;
    Node := LoadX3DClassicStream(TempStream);

    { make sure saved and loaded back Ok }
    AssertTrue(Node.HasForceVersion);
    AssertTrue(Node.ForceVersion.Major = 3);
    AssertTrue(Node.ForceVersion.Minor = 1);
    AssertTrue(Node.Profile = 'Immersive');
    AssertTrue(Node.Components.Count = 2);
    AssertTrue(Node.Components['NURBS'] = 2);
    AssertTrue(Node.Components['Shaders'] = 1);
    AssertTrue(Node.Meta.Count = 2);
    AssertTrue(Node.Meta['test''''key'] = 'test"value');
    AssertTrue(Node.Meta['generator'] = 'testgenerator and & weird '' chars " test');

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

    { save and load again. During Save3D tweak meta generator and source }
    TempStream.Position := 0;
    Save3D(Node, TempStream, 'newgenerator', 'newsource', xeClassic, false);
    FreeAndNil(Node);
    TempStream.Position := 0;
    Node := LoadX3DClassicStream(TempStream);

    { make sure saved and loaded back Ok }
    AssertTrue(Node.HasForceVersion);
    AssertTrue(Node.ForceVersion.Major = 3);
    AssertTrue(Node.ForceVersion.Minor = 1);
    AssertTrue(Node.Profile = 'Immersive');
    AssertTrue(Node.Components.Count = 2);
    AssertTrue(Node.Components['NURBS'] = 2);
    AssertTrue(Node.Components['Shaders'] = 1);
    AssertTrue(Node.Meta.Count = 6);
    AssertTrue(Node.Meta['test''''key'] = 'newvalue');
    AssertTrue(Node.Meta['testkey2'] = 'evennewervalue2');
    AssertTrue(Node.Meta['testkey3'] = 'newvalue3');
    AssertTrue(Node.Meta['generator'] = 'newgenerator');
    AssertTrue(Node.Meta['generator-previous'] = 'testgenerator and & weird '' chars " test');
    AssertTrue(Node.Meta['source'] = 'newsource');

    { save and load again, this time going through XML }
    TempStream.Position := 0;
    Save3D(Node, TempStream, '', '', xeXML, false);
    FreeAndNil(Node);
    TempStream.Position := 0;
    Node := LoadX3DXml(TempStream, '');

    { make sure saved and loaded back Ok }
    AssertTrue(Node.HasForceVersion);
    AssertTrue(Node.ForceVersion.Major = 3);
    AssertTrue(Node.ForceVersion.Minor = 1);
    AssertTrue(Node.Profile = 'Immersive');
    AssertTrue(Node.Components.Count = 2);
    AssertTrue(Node.Components['NURBS'] = 2);
    AssertTrue(Node.Components['Shaders'] = 1);
    AssertTrue(Node.Meta.Count = 6);
    AssertTrue(Node.Meta['test''''key'] = 'newvalue');
    AssertTrue(Node.Meta['testkey2'] = 'evennewervalue2');
    AssertTrue(Node.Meta['testkey3'] = 'newvalue3');
    AssertTrue(Node.Meta['generator'] = 'newgenerator');
    AssertTrue(Node.Meta['generator-previous'] = 'testgenerator and & weird '' chars " test');
    AssertTrue(Node.Meta['source'] = 'newsource');

  finally
    FreeAndNil(Node);
    FreeAndNil(TempStream);
  end;
end;

procedure TTestX3DNodes.TestConvertToX3D;
var
  Node: TX3DRootNode;
  TempStream: TMemoryStream;
begin
  TempStream := nil;
  Node := nil;

  try
    TempStream := TMemoryStream.Create;

    { load X3D 3.1 }
    Node := LoadX3DClassicFromString('#X3D V3.1 utf8' +NL+
      'PROFILE Immersive', '');
    AssertTrue(Node.HasForceVersion = true);
    AssertTrue(Node.ForceVersion.Major = 3);
    AssertTrue(Node.ForceVersion.Minor = 1);

    { save to XML }
    TempStream.Position := 0;
    TempStream.Size := 0;
    Save3D(Node, TempStream, '', '', xeXML, true);
    FreeAndNil(Node);

    { check that loading it back results in 3.1 }
    TempStream.Position := 0;
    Node := LoadX3DXml(TempStream, '');
    AssertTrue(Node.HasForceVersion = true);
    AssertTrue(Node.ForceVersion.Major = 3);
    AssertTrue(Node.ForceVersion.Minor = 1);

    { save to clasic }
    TempStream.Position := 0;
    TempStream.Size := 0;
    Save3D(Node, TempStream, '', '', xeClassic, true);
    FreeAndNil(Node);

    { check that loading it back results in 3.1 }
    TempStream.Position := 0;
    Node := LoadX3DClassicStream(TempStream);
    AssertTrue(Node.HasForceVersion = true);
    AssertTrue(Node.ForceVersion.Major = 3);
    AssertTrue(Node.ForceVersion.Minor = 1);
    FreeAndNil(Node);

    { load VRML 2.0 }
    Node := LoadX3DClassicFromString('#VRML V2.0 utf8' + NL, '');
    AssertTrue(Node.HasForceVersion = true);
    AssertTrue(Node.ForceVersion.Major = 2);
    AssertTrue(Node.ForceVersion.Minor = 0);

    { save to XML }
    TempStream.Position := 0;
    TempStream.Size := 0;
    Save3D(Node, TempStream, '', '', xeXML, false);
    FreeAndNil(Node);

    { check that loading it back results in 3.0
      (conversion was done, since this is XML) }
    TempStream.Position := 0;
    Node := LoadX3DXml(TempStream, '');
    AssertTrue(Node.HasForceVersion = true);
    AssertTrue(Node.ForceVersion.Major = 3);
    AssertTrue(Node.ForceVersion.Minor = 0);
    FreeAndNil(Node);

    { load VRML 2.0 }
    Node := LoadX3DClassicFromString('#VRML V2.0 utf8' + NL, '');
    AssertTrue(Node.HasForceVersion = true);
    AssertTrue(Node.ForceVersion.Major = 2);
    AssertTrue(Node.ForceVersion.Minor = 0);

    { save to classic }
    TempStream.Position := 0;
    TempStream.Size := 0;
    Save3D(Node, TempStream, '', '', xeClassic, false);
    FreeAndNil(Node);

    { check that loading it back results in 2.0
      (conversion not done, since this is classic and conversion not forced) }
    TempStream.Position := 0;
    Node := LoadX3DClassicStream(TempStream);
    AssertTrue(Node.HasForceVersion = true);
    AssertTrue(Node.ForceVersion.Major = 2);
    AssertTrue(Node.ForceVersion.Minor = 0);
    FreeAndNil(Node);

    { load VRML 2.0 }
    Node := LoadX3DClassicFromString('#VRML V2.0 utf8' + NL, '');
    AssertTrue(Node.HasForceVersion = true);
    AssertTrue(Node.ForceVersion.Major = 2);
    AssertTrue(Node.ForceVersion.Minor = 0);

    { save to classic }
    TempStream.Position := 0;
    TempStream.Size := 0;
    Save3D(Node, TempStream, '', '', xeClassic, true);
    FreeAndNil(Node);

    { check that loading it back results in 3.0
      (conversion done, since forced = true) }
    TempStream.Position := 0;
    Node := LoadX3DClassicStream(TempStream);
    AssertTrue(Node.HasForceVersion = true);
    AssertTrue(Node.ForceVersion.Major = 3);
    AssertTrue(Node.ForceVersion.Minor = 0);
    FreeAndNil(Node);
  finally
    FreeAndNil(Node);
    FreeAndNil(TempStream);
  end;
end;

procedure TTestX3DNodes.TestReadingWritingQuotes;
const
  ValidString = 'test string with " and '' and \ and / inside';
  ValidString2 = '" another '''' test string  with some weirdness \\ inside';

  function CreateTestScene: TX3DRootNode;
  var
    Text: TTextNode;
    Shape: TShapeNode;
    Touch: TTouchSensorNode;
  begin
    Text := TTextNode.Create;
    Text.FdString.Items.Add(ValidString);
    Text.FdString.Items.Add(ValidString2);

    Shape := TShapeNode.Create;
    Shape.FdGeometry.Value := Text;

    Touch := TTouchSensorNode.Create;
    Touch.FdDescription.Value := ValidString;

    Result := TX3DRootNode.Create;
    Result.AddChildren(Shape);
    Result.AddChildren(Touch);
  end;

  procedure Assertions(Node: TX3DRootNode);
  var
    StringField: TMFString;
  begin
    StringField := ((Node.FdChildren[0] as TShapeNode).FdGeometry.Value as TTextNode).FdString;
    AssertTrue(StringField.Count = 2);
    AssertTrue(StringField.Items[0] = ValidString);
    AssertTrue(StringField.Items[1] = ValidString2);
    AssertTrue((Node.FdChildren[1] as TTouchSensorNode).FdDescription.Value = ValidString);
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
    Save3D(Node, TempStream, '', '', xeClassic, true);
    FreeAndNil(Node);

    TempStream.Position := 0;
    Node := LoadX3DClassicStream(TempStream);
    Assertions(Node);

    TempStream.Position := 0;
    TempStream.Size := 0;
    Save3D(Node, TempStream, '', '', xeXML, true);
    FreeAndNil(Node);

    TempStream.Position := 0;
    Node := LoadX3DXml(TempStream, '');
    Assertions(Node);
  finally
    FreeAndNil(Node);
    FreeAndNil(TempStream);
  end;
end;

procedure TTestX3DNodes.TestSolid;
var
  IFS: TIndexedFaceSetNode;
  LineSet: TLineSetNode;
begin
  IFS := TIndexedFaceSetNode.Create;
  try
    AssertTrue(IFS.FdSolid.Value);
    AssertTrue(IFS.SolidField.Value);
    AssertTrue(IFS.Solid);

    IFS.Solid := false;
    AssertTrue(not IFS.FdSolid.Value);
    AssertTrue(not IFS.SolidField.Value);
    AssertTrue(not IFS.Solid);
  finally FreeAndNil(IFS) end;

  // LineSet doesn't have FdSolid field, but still Solid property should exist
  LineSet := TLineSetNode.Create;
  try
    //AssertTrue(LineSet.FdSolid.Value);
    AssertTrue(LineSet.SolidField = nil);
    AssertTrue(LineSet.Solid);

    LineSet.Solid := false;
    //AssertTrue(not LineSet.FdSolid.Value);
    AssertTrue(LineSet.SolidField = nil);
    AssertTrue(not LineSet.Solid);
  finally FreeAndNil(LineSet) end;
end;

procedure TTestX3DNodes.TestConvex;
var
  IFS: TIndexedFaceSetNode;
  LineSet: TLineSetNode;
begin
  IFS := TIndexedFaceSetNode.Create;
  try
    AssertTrue(IFS.FdConvex.Value);
    AssertTrue(IFS.ConvexField.Value);
    AssertTrue(IFS.Convex);

    IFS.Convex := false;
    AssertTrue(not IFS.FdConvex.Value);
    AssertTrue(not IFS.ConvexField.Value);
    AssertTrue(not IFS.Convex);
  finally FreeAndNil(IFS) end;

  // LineSet doesn't have FdConvex field, but still Convex property should exist
  LineSet := TLineSetNode.Create;
  try
    //AssertTrue(LineSet.FdConvex.Value);
    AssertTrue(LineSet.ConvexField = nil);
    AssertTrue(LineSet.Convex);

    LineSet.Convex := false;
    //AssertTrue(not LineSet.FdConvex.Value);
    AssertTrue(LineSet.ConvexField = nil);
    AssertTrue(not LineSet.Convex);
  finally FreeAndNil(LineSet) end;
end;

procedure TTestX3DNodes.TestX3DXmlString;
begin
  AssertTrue(StringToX3DXml(
    'castlescript:' + #10 +
    'function initialize(time)' + #10 +
    '  { set up first thunder in the future }' + #10 +
    '  startLight1AndAudio :=  time +' + #10 +
    '    durationBetweenConst + random() * durationBetweenRandom1;' + #10 +
    '  startLight2 := startLight1AndAudio + blink2Start' + #10 +
    '' + #10 +
    'function forceThunderNow(value, time)' + #10 +
    '  when (value,' + #10 +
    '    startLight1AndAudio := time;' + #10 +
    '    startLight2 := startLight1AndAudio + blink2Start)' + #10 +
    '' + #10 +
    'function light2Active(value, time)' + #10 +
    '  when (and(not(value), not(audioActive)),' + #10 +
    '    { Once everything finished (2nd light blink and sound) finished,' + #10 +
    '      set up next thunder in the future.' + #10 +
    '      We can only do it once everything finished, as X3D spec says that' + #10 +
    '      ''Any set_startTime events to an active time-dependent node are ignored.'' }' + #10 +
    '    startLight1AndAudio := startLight1AndAudio +' + #10 +
    '      durationBetweenConst + random() * durationBetweenRandom2;' + #10 +
    '    startLight2 := startLight1AndAudio + blink2Start)' + #10 +
    '' + #10 +
    'function audioActive(value, time)' + #10 +
    '  when (and(not(value), not(light2Active)),' + #10 +
    '    { Exactly like light2Active. We have to watch for both light2Active' + #10 +
    '      and audioActive, as we don''t know which one takes longer: light blinking' + #10 +
    '      or audio sound. }' + #10 +
    '    startLight1AndAudio := startLight1AndAudio +' + #10 +
    '      durationBetweenConst + random() * durationBetweenRandom2;' + #10 +
    '    startLight2 := startLight1AndAudio + blink2Start)' + #10 +
    '') =
    '"castlescript:&#xA;function initialize(time)&#xA;  { set up first thunder in the future }&#xA;  startLight1AndAudio :=  time +&#xA;    durationBetweenConst + random() * durationBetweenRandom1;&#xA;  startLight2 := startLight1AndAudio + ' + 'blink2Start&#xA;&#xA;function forceThunderNow(value, time)&#xA;  when (value,&#xA;    startLight1AndAudio := time;&#xA;    startLight2 := startLight1AndAudio + blink2Start)&#xA;&#xA;function light2Active(value, time)&#xA;  when (and(not(value), not(audioActive)),&#xA;    { Once everything finished (2nd light blink and sound) finished,&#xA;      set up next thunder in the future.&#xA;      We can only do it once everything finished, as X3D spec says that&#xA;      &apos;Any set_startTime events to an active time-dependent node are ignored.&apos; }&#xA;    startLight1AndAudio := startLight1AndAudio +&#xA;      durationBetweenConst + random() * durationBetweenRandom2;&#xA;    startLight2 := startLight1AndAudio + blink2Start)&#xA;&#xA;function ' + 'audioActive(value, time)&#xA;  when (and(not(value), not(light2Active)),&#xA;    { Exactly like light2Active. We have to watch for both light2Active&#xA;      and audioActive, as we don&apos;t know which one takes longer: light blinking&#xA;      or audio sound. }&#xA;    startLight1AndAudio := startLight1AndAudio +&#xA;      durationBetweenConst + random() * durationBetweenRandom2;&#xA;    startLight2 := startLight1AndAudio + blink2Start)&#xA;"');
end;

procedure TTestX3DNodes.TestOrthoViewpointFieldOfView;
var
  O: TOrthoViewpointNode;
begin
  O := TOrthoViewpointNode.Create;
  try
    AssertEquals(4, O.FdFieldOfView.Count);
    AssertEquals(-1, O.FdFieldOfView.Items[0]);
    AssertEquals(-1, O.FdFieldOfView.Items[1]);
    AssertEquals( 1, O.FdFieldOfView.Items[2]);
    AssertEquals( 1, O.FdFieldOfView.Items[3]);

    AssertEquals(-1, O.FieldOfView[0]);
    AssertEquals(-1, O.FieldOfView[1]);
    AssertEquals( 1, O.FieldOfView[2]);
    AssertEquals( 1, O.FieldOfView[3]);

    AssertEquals(-1, O.FieldOfViewMinX);
    AssertEquals(-1, O.FieldOfViewMinY);
    AssertEquals( 1, O.FieldOfViewMaxX);
    AssertEquals( 1, O.FieldOfViewMaxY);

    O.FdFieldOfView.Items.Clear;
    AssertEquals(0, O.FdFieldOfView.Count);

    AssertEquals(-1, O.FieldOfView[0]);
    AssertEquals(-1, O.FieldOfView[1]);
    AssertEquals( 1, O.FieldOfView[2]);
    AssertEquals( 1, O.FieldOfView[3]);

    AssertEquals(-1, O.FieldOfViewMinX);
    AssertEquals(-1, O.FieldOfViewMinY);
    AssertEquals( 1, O.FieldOfViewMaxX);
    AssertEquals( 1, O.FieldOfViewMaxY);

    O.FieldOfViewMaxX := 10;

    AssertEquals(-1, O.FieldOfView[0]);
    AssertEquals(-1, O.FieldOfView[1]);
    AssertEquals(10, O.FieldOfView[2]);
    AssertEquals( 1, O.FieldOfView[3]);

    AssertEquals(-1, O.FieldOfViewMinX);
    AssertEquals(-1, O.FieldOfViewMinY);
    AssertEquals(10, O.FieldOfViewMaxX);
    AssertEquals( 1, O.FieldOfViewMaxY);

    AssertEquals(3, O.FdFieldOfView.Count);
    AssertEquals(-1, O.FdFieldOfView.Items[0]);
    AssertEquals(-1, O.FdFieldOfView.Items[1]);
    AssertEquals(10, O.FdFieldOfView.Items[2]);

    O.FieldOfViewMaxY := 20;

    AssertEquals(-1, O.FieldOfView[0]);
    AssertEquals(-1, O.FieldOfView[1]);
    AssertEquals(10, O.FieldOfView[2]);
    AssertEquals(20, O.FieldOfView[3]);

    AssertEquals(-1, O.FieldOfViewMinX);
    AssertEquals(-1, O.FieldOfViewMinY);
    AssertEquals(10, O.FieldOfViewMaxX);
    AssertEquals(20, O.FieldOfViewMaxY);

    AssertEquals(4, O.FdFieldOfView.Count);
    AssertEquals(-1, O.FdFieldOfView.Items[0]);
    AssertEquals(-1, O.FdFieldOfView.Items[1]);
    AssertEquals(10, O.FdFieldOfView.Items[2]);
    AssertEquals(20, O.FdFieldOfView.Items[3]);

    O.FieldOfViewMinY := -20;

    AssertEquals(-1, O.FieldOfView[0]);
    AssertEquals(-20, O.FieldOfView[1]);
    AssertEquals(10, O.FieldOfView[2]);
    AssertEquals(20, O.FieldOfView[3]);

    AssertEquals(-1, O.FieldOfViewMinX);
    AssertEquals(-20, O.FieldOfViewMinY);
    AssertEquals(10, O.FieldOfViewMaxX);
    AssertEquals(20, O.FieldOfViewMaxY);

    AssertEquals(4, O.FdFieldOfView.Count);
    AssertEquals(-1, O.FdFieldOfView.Items[0]);
    AssertEquals(-20, O.FdFieldOfView.Items[1]);
    AssertEquals(10, O.FdFieldOfView.Items[2]);
    AssertEquals(20, O.FdFieldOfView.Items[3]);

  finally FreeAndNil(O) end;
end;

procedure TTestX3DNodes.TestFontStyle;
var
  F: TFontStyleNode;
begin
  F := TFontStyleNode.Create;
  try
    AssertEquals(false, F.Bold);
    AssertEquals(false, F.Italic);
    AssertTrue(ffSerif = F.Family);
    AssertTrue(fjBegin = F.Justify);
    AssertTrue(fjFirst = F.JustifyMinor);

    F.Bold := true;
    AssertEquals(true, F.Bold);
    AssertEquals(false, F.Italic);
    AssertEquals('BOLD', F.FdStyle.Value);

    F.Italic := true;
    AssertEquals(true, F.Bold);
    AssertEquals(true, F.Italic);
    AssertEquals('BOLDITALIC', F.FdStyle.Value);

    F.Bold := false;
    AssertEquals(false, F.Bold);
    AssertEquals(true, F.Italic);
    AssertEquals('ITALIC', F.FdStyle.Value);

    F.JustifyMinor := fjEnd;
    AssertTrue(fjBegin = F.Justify);
    AssertTrue(fjEnd = F.JustifyMinor);

    F.Justify := fjMiddle;
    AssertTrue(fjMiddle = F.Justify);
    AssertTrue(fjEnd = F.JustifyMinor);

    F.Family := ffSans;
    AssertTrue(ffSans = F.Family);

  finally FreeAndNil(F) end;
end;

type
  EWeakLinkUnused = class(Exception);

procedure TTestX3DNodes.WeakLinkUnusedWarning(Sender: TObject; const Category, S: string);
begin
  if Pos('GeneratedShadowMap.light', S) <> 0 then
    raise EWeakLinkUnused.Create('We want this warning, good: ' + S)
  else
    raise Exception.Create('Some invalid warning: ' + S);
end;

procedure TTestX3DNodes.TestWeakLinkUnusedWarning;
begin
  ApplicationProperties.OnWarning.Add(@WeakLinkUnusedWarning);
  try
    try
      Load3D(ApplicationData('warning_when_new_node_as_shadow_map_light.x3dv'));
      raise Exception.Create('We should not get here, expected EWeakLinkUnused on the way');
    except
      on EWeakLinkUnused do ; { good, silence this for the sake of test }
    end;
  finally
    ApplicationProperties.OnWarning.Remove(@WeakLinkUnusedWarning);
  end;
end;

procedure TTestX3DNodes.TestKeepExisting;
var
  Texture: TImageTextureNode;
  TextureProperties: TTexturePropertiesNode;
begin
  TextureProperties := TTexturePropertiesNode.Create;
  TextureProperties.AnisotropicDegree := 8;
  TextureProperties.MinificationFilter := minFastest;
  AssertTrue(TextureProperties.MinificationFilter = minFastest);
  TextureProperties.MagnificationFilter := magNicest;
  AssertTrue(TextureProperties.MagnificationFilter = magNicest);
  TextureProperties.KeepExisting := 1;

  Texture := TImageTextureNode.Create;
  Texture.FdTextureProperties.Value := TextureProperties;
  FreeAndNil(Texture);

  { This *will* cause SIGSEGV without KeepExisting := 1 above.
    But it will work fine with KeepExisting := 1 above. }
  FreeAndNil(TextureProperties);
end;

procedure TTestX3DNodes.TestAttenuation;
var
  L: TPointLightNode;
begin
  L := TPointLightNode.Create;
  try
    AssertFalse(L.DistanceNeededForAttenuation);
    AssertEquals(1, L.CalculateAttenuation(123));

    { VRML 97 specification says that attenuation = (0, 0, 0) should
      behave like (1, 0, 0). }
    L.Attenuation := TVector3.Zero;
    AssertFalse(L.DistanceNeededForAttenuation);
    AssertEquals(1, L.CalculateAttenuation(123));

    L.Attenuation := Vector3(1, 1, 1);
    AssertTrue(L.DistanceNeededForAttenuation);
    AssertSameValue(1 / (1 + 5 + Sqr(5)), L.CalculateAttenuation(5), 0.001);
  finally FreeAndNil(L) end;
end;

procedure TTestX3DNodes.TestAddChildren;
var
  M: TMaterialNode;
  S: TShapeNode;
  G: TGroupNode;
  FieldToSend: TMFNode;
begin
  G := TGroupNode.Create;
  try
    AssertEquals(0, G.FdChildren.Count);

    // adding using AddChildren method works

    S := TShapeNode.Create;
    G.AddChildren([S, S]);
    AssertEquals(1, G.FdChildren.Count);
    G.AddChildren([S, S, S]);
    AssertEquals(1, G.FdChildren.Count);
    // only one instance of TShapeNode is added

    S := TShapeNode.Create;
    G.AddChildren([S, S]);
    AssertEquals(2, G.FdChildren.Count);

    S := TShapeNode.Create;
    G.AddChildren(S);
    AssertEquals(3, G.FdChildren.Count);
    G.RemoveChildren(S);
    AssertEquals(2, G.FdChildren.Count);

    FieldToSend := TMFNode.CreateUndefined(nil, false, 'temporary');
    try
      S := TShapeNode.Create;
      FieldToSend.Add(S);
      S := TShapeNode.Create;
      FieldToSend.Add(S);
      G.EventAddChildren.Send(FieldToSend, TX3DTime.Oldest);

      // adding through EventAddChildren works
      AssertEquals(4, G.FdChildren.Count);
    finally FreeAndNil(FieldToSend) end;

    FieldToSend := TMFNode.CreateUndefined(nil, false, 'temporary');
    try
      M := TMaterialNode.Create;
      FieldToSend.Add(M);
      G.EventAddChildren.Send(FieldToSend, TX3DTime.Oldest);

      // adding Material is ignored, not a child node
      AssertEquals(4, G.FdChildren.Count);
    finally FreeAndNil(FieldToSend) end;
  finally FreeAndNil(G) end;
end;

procedure TTestX3DNodes.TestNurbsCurvePoint;
var
  CurveNode: TNurbsCurveNode;
  Coordinate: TCoordinateNode;
begin
  Coordinate := TCoordinateNode.Create;
  Coordinate.SetPoint([
    Vector3(2.285389, 1.235778, 1.636090),
    Vector3(1, 0, 0),
    Vector3(1.141864, 1.003204, -1.775073),
    Vector3(1, 0, 0),
    Vector3(3.120634, 1.865495, 2.322197)
  ]);

  CurveNode := TNurbsCurveNode.Create;
  CurveNode.ControlPoint := Coordinate;

  AssertVectorEquals(Vector3(2.285389, 1.235778, 1.636090), CurveNode.Point(0));
  AssertVectorEquals(Vector3(3.120634, 1.865495, 2.322197), CurveNode.Point(1));

  FreeAndNil(CurveNode);
end;

initialization
 RegisterTest(TTestX3DNodes);
end.
