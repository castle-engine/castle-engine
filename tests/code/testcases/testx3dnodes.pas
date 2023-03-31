// -*- compile-command: "./test_single_testcase.sh TTestX3DNodes" -*-
{
  Copyright 2004-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test X3DNodes unit. }
unit TestX3DNodes;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif}, CastleVectors, X3DNodes;

type
  TTestX3DNodes = class(TCastleTestCase)
  private
    procedure WeakLinkUnusedWarning(const Category, S: string);
  protected
    { Every possible X3D nodes that makes no errors when instantiated.
      Above NodesManager, this also includes some abstract node classes
      (not to be used in X3D files, but that make sense to instantiate)
      and TX3DRootNode (non-abstract (Pascal code can create it)
      but also not available in X3D files explicitly). }
    InstantiableNodes: TX3DNodeClassesList;
    procedure SetUp; override;
    procedure TearDown; override;
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
    procedure TestInheritsFrom;
    procedure TestIndexOfAnyAncestor;
    procedure TestAllowedChildren;
    procedure TestContainerFieldList;
    procedure TestContainerFieldGeometry;
    procedure TestDestructionNotification;

    procedure TestGeometryNodesImplemented;

    { Test all geometry nodes should have Change = chGeometryXxx
      on all fields (except "metadata").
      All non-geometry nodes should not have chGeometryXxx on any field. }
    procedure TestGeometryNodesChanges;

    { All Color nodes should have Changes = [chColorNode] }
    procedure TestColorNodeChanges;

    { All tex coord nodes should have Change = [chTextureCoordinate] }
    procedure TestTextureCoordinate;

    { Try getting TTimeDependentFunctionality and use the handler.
      Catches e.g. not overriden CycleInterval. }
    procedure TestTimeDependentFunctionality;

    procedure TestTransformFunctionality;
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
    procedure TestAddChildrenAllowDuplicates;
    procedure TestMetadata;
    procedure TestMetadataArray;
    procedure TestNiceName;
    procedure TestTextureProperties;
    procedure TestFixNames;
    procedure TestAutomaticWeakLink;
    procedure TestNonGenericFind;
    {$ifdef GENERIC_METHODS}
    procedure TestGenericFind;
    {$endif}
    procedure TestProtoExpansion;
    procedure TestSolidField;
  end;

implementation

uses Generics.Collections, Math,
  CastleUtils, CastleInternalX3DLexer, CastleClassUtils, CastleFilesUtils,
  X3DFields, CastleTimeUtils, CastleDownload, X3DLoad, X3DTime, CastleColors,
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

procedure TTestX3DNodes.SetUp;
var
  I: Integer;
begin
  inherited;

  InstantiableNodes := TX3DNodeClassesList.Create;

  for I := 0 to NodesManager.RegisteredCount - 1 do
    InstantiableNodes.Add(NodesManager.Registered[I]);

  InstantiableNodes.Add(TX3DRootNode);
  InstantiableNodes.Add(TX3DNode);
  InstantiableNodes.Add(TAbstractInternalGroupingNode);
  InstantiableNodes.Add(TAbstractGroupingNode);
end;

procedure TTestX3DNodes.TearDown;
begin
  FreeAndNil(InstantiableNodes);
  inherited;
end;

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

  TX3DTokenInfoList = class({$ifdef FPC}specialize{$endif} TObjectList<TX3DTokenInfo>)
    procedure AssertEqual(const TestCase: {$ifndef CASTLE_TESTER}TTestCase{$else}TCastleTestCase{$endif}; SecondValue: TX3DTokenInfoList);
    procedure ReadFromFile(const FileName: string);
  end;

procedure TX3DTokenInfoList.AssertEqual(const TestCase: {$ifndef CASTLE_TESTER}TTestCase{$else}TCastleTestCase{$endif};
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
        vtFloat: result := result +' ' +FloatToStrDot(T.Float);
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
  1. invalid X3D files (that use some funny node names)
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

      Node := LoadX3DClassic(FileName, false);
      NewFile := InclPathDelim({$ifndef CASTLE_TESTER}GetTempDir{$else}GetTempDirectory{$endif}) + 'test_castle_game_engine.x3dv';
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

  // TestReadWrite('castle-data:/demo-models-copy/proto_sfnode_default.x3dv');
  // TestReadWrite('castle-data:/demo-models-copy/tricky_def_use.x3dv');
end;

procedure TTestX3DNodes.TestInheritsFrom;
begin
  AssertTrue(TGroupNode_2.InheritsFrom(TAbstractChildNode));
  AssertTrue(TSwitchNode_2.InheritsFrom(TAbstractChildNode));
  AssertFalse(TConeNode_2.InheritsFrom(TAbstractChildNode));
  AssertFalse(TAppearanceNode.InheritsFrom(TAbstractChildNode));
  AssertFalse(TX3DNode.InheritsFrom(TAbstractChildNode));
  AssertFalse(TObject.InheritsFrom(TAbstractChildNode));
end;

procedure TTestX3DNodes.TestIndexOfAnyAncestor;
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
  L := TX3DNodeClassesList.Create;
  try
    L.Add(TAbstractChildNode);
    { Test L.IndexOfAnyAncestor }
    AssertTrue(IndexOfAnyAncestorByClass(TGroupNode_2));
    AssertTrue(IndexOfAnyAncestorByClass(TSwitchNode_2));
    AssertFalse(IndexOfAnyAncestorByClass(TConeNode_2));
    AssertFalse(IndexOfAnyAncestorByClass(TAppearanceNode));
    AssertFalse(IndexOfAnyAncestorByClass(TX3DNode));
  finally FreeAndNil(L) end;
end;

procedure TTestX3DNodes.TestUniqueFields;
var
  I, J, K: Integer;
  N: TX3DNode;
  CurrentName: string;
begin
  for I := 0 to InstantiableNodes.Count - 1 do
  begin
    N := InstantiableNodes[I].Create;
    try
      { Test that all fields, events names are different.

        Doesn't detect if two alternative names match each other for now!
        (Although will detect if some alternative name will match non-alternative
        name, since uses IsName comparison).

        Also, doesn't check the implicitly exposed events for now. }

      for J := 0 to N.FieldsCount - 1 do
      begin
        CurrentName := N.Fields[J].X3DName;
        for K := 0 to N.FieldsCount - 1 do
          AssertTrue(N.X3DType + '.' + CurrentName + ' must be unique field name',
            (K = J) or (not N.Fields[K].IsName(CurrentName)));
        for K := 0 to N.EventsCount - 1 do
          AssertTrue(N.X3DType + '.' + CurrentName + ' must be unique event name',
            not N.Events[K].IsName(CurrentName));
      end;

      for J := 0 to N.EventsCount - 1 do
      begin
        CurrentName := N.Events[J].X3DName;
        for K := 0 to N.FieldsCount - 1 do
          AssertTrue(N.X3DType + '.' + CurrentName + ' must be unique field name',
            not N.Fields[K].IsName(CurrentName));
        for K := 0 to N.EventsCount - 1 do
          AssertTrue(N.X3DType + '.' + CurrentName + ' must be unique event name',
            (K = J) or (not N.Events[K].IsName(CurrentName)));
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
    all children nodes simply inherit from TAbstractChildNode.
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
    TIndexedTriangleMeshNode_1,
    TRotationXYZNode_1,

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
    {$ifdef FPC}TScriptNode,{$endif}
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
    TCylinderNode,
    TElevationGridNode,
    TExtrusionNode,
    TGeoElevationGridNode,
    TIndexedFaceSetNode,
    TIndexedLineSetNode,
    TNurbsCurveNode,
    TPointSetNode,
    TSphereNode,
    TTextNode,
    TText3DNode
  ]);

  try
    for I := 0 to AllowedChildrenNodes.Count - 1 do
    try
      AssertTrue(
        // this check corresponds to TX3DRootNode.FdChildren constraints
        AllowedChildrenNodes[I].InheritsFrom(TAbstractChildNode) or
        AllowedChildrenNodes[I].InheritsFrom(TAbstractGeometryNode_1)
      );

      { Just to make sure, check also the created class
        (I don't trust FPC interfaces for now...) }
      N := AllowedChildrenNodes[I].Create;
      try
        AssertTrue(
          (N is TAbstractChildNode) or
          (N is TAbstractGeometryNode_1)
        );
      finally FreeAndNil(N) end;
    except
      on E: Exception do
      begin
        Writeln('Failed on ', AllowedChildrenNodes[I].ClassName, ' is IAbstractChildNode|TAbstractGeometryNode_1');
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

    for I := 0 to InstantiableNodes.Count - 1 do
    begin
      N := InstantiableNodes[I].Create;
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
  for I := 0 to InstantiableNodes.Count - 1 do
  begin
    N := InstantiableNodes[I].Create;
    try
      if N is TAbstractGeometryNode then
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
    procedure Foo(const Node: TX3DNode);
  end;

procedure TMyObject.Foo(const Node: TX3DNode);
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
    A.Add({$ifdef FPC}@{$endif}M1.Foo);
    A.Add({$ifdef FPC}@{$endif}M2.Foo);
    A.Add({$ifdef FPC}@{$endif}M3.Foo);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}M1.Foo) = 0);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}M2.Foo) = 1);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}M3.Foo) = 2);
    A.Remove({$ifdef FPC}@{$endif}M2.Foo);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}M1.Foo) = 0);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}M2.Foo) = -1);
    AssertTrue(A.IndexOf({$ifdef FPC}@{$endif}M3.Foo) = 1);
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
    for I := 0 to InstantiableNodes.Count - 1 do
    begin
      N := InstantiableNodes[I].Create;
      try
        if N is TAbstractGeometryNode then
        try
          G := TAbstractGeometryNode(N);

          { test proxy may be created }
          ProxyState := State;
          ProxyGeometry := G.Proxy(ProxyState);

          { test that methods are overriden correctly, and don't crash }
          G.BoundingBox(State, ProxyGeometry, ProxyState);
          G.LocalBoundingBox(State, ProxyGeometry, ProxyState);
          G.VerticesCount(State, ProxyGeometry, ProxyState);
          G.TrianglesCount(State, ProxyGeometry, ProxyState);

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
  for I := 0 to InstantiableNodes.Count - 1 do
  begin
    N := InstantiableNodes[I].Create;
    try
      if N is TAbstractGeometryNode then
      begin
        for J := 0 to N.FieldsCount - 1 do
          if N.Fields[J].X3DName <> 'metadata' then
            AssertTrue(
              Format('Geometry node %s, field %s, must use chGeometryXxx', [N.X3DType, N.Fields[J].X3DName]),
              N.Fields[J].ExecuteChange in [chGeometry, chGeometryFontChanged]);
      end else
      begin
        for J := 0 to N.FieldsCount - 1 do
          AssertTrue(
            Format('Non-geometry node %s, field %s, must NOT use chGeometryXxx', [N.X3DType, N.Fields[J].X3DName]),
            not (N.Fields[J].ExecuteChange in [chGeometry, chGeometryFontChanged]));
      end

    finally FreeAndNil(N) end;
  end;
end;

procedure TTestX3DNodes.TestColorNodeChanges;
var
  I, J: Integer;
  N: TX3DNode;
begin
  for I := 0 to InstantiableNodes.Count - 1 do
  begin
    N := InstantiableNodes[I].Create;
    try
      if N is TAbstractColorNode then
      begin
        for J := 0 to N.FieldsCount - 1 do
          if (N.Fields[J].X3DName <> 'metadata') and
             (N.Fields[J].X3DName <> 'mode') then
          try
            AssertTrue(N.Fields[J].ExecuteChange = chColorNode);
          except
            Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].X3DName);
            raise;
          end;
      end else
      begin
        for J := 0 to N.FieldsCount - 1 do
        try
          AssertTrue(chColorNode <> N.Fields[J].ExecuteChange);
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
  for I := 0 to InstantiableNodes.Count - 1 do
  begin
    N := InstantiableNodes[I].Create;
    try
      if N is TAbstractTextureCoordinateNode then
      begin
        for J := 0 to N.FieldsCount - 1 do
          if N.Fields[J].X3DName <> 'metadata' then
          try
            AssertTrue(N.Fields[J].ExecuteChange = chTextureCoordinate);
          except
            Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].X3DName);
            raise;
          end;
      end else
      begin
        for J := 0 to N.FieldsCount - 1 do
        try
          AssertTrue(chTextureCoordinate <> N.Fields[J].ExecuteChange);
        except
          Writeln('Failed on ', N.ClassName, ', field ', N.Fields[J].X3DName);
          raise;
        end;
      end

    finally FreeAndNil(N) end;
  end;
end;

procedure TTestX3DNodes.TestTimeDependentFunctionality;

  procedure CheckTimeDependentFunctionality(const N: TX3DNode);
  var
    B: boolean;
    C: TFloatTime;
    F: TTimeDependentFunctionality;
  begin
    AssertTrue(
      N.Functionality(TTimeDependentFunctionality) = N.TimeFunctionality);

    if (N is TAbstractTimeDependentNode) or
       // this lists some types that should descend already from TAbstractTimeDependentNode
       (N is TMovieTextureNode) or
       (N is TAudioClipNode) or
       (N is TTimeSensorNode) then
      AssertTrue(N.Functionality(TTimeDependentFunctionality) <> nil);

    F := N.Functionality(TTimeDependentFunctionality) as TTimeDependentFunctionality;
    if F <> nil then
    begin
      B := F.IsActive;
      C := F.CycleInterval;
    end;
  end;

var
  I: Integer;
  N: TX3DNode;
begin
  for I := 0 to InstantiableNodes.Count - 1 do
  begin
    N := InstantiableNodes[I].Create;
    try
      CheckTimeDependentFunctionality(N);
    except
      Writeln('TestTimeDependentFunctionality failed for ', N.ClassName);
      raise;
    end;
    FreeAndNil(N);
  end;
end;

procedure TTestX3DNodes.TestTransformFunctionality;

  function ContainsCHTransformField(const N: TX3DNode): boolean;
  var
    I: Integer;
  begin
    for I := 0 to N.FieldsCount - 1 do
      if chTransform = N.Fields[I].ExecuteChange then
        Exit(true);
    Result := false;
  end;

var
  I: Integer;
  N: TX3DNode;
begin
  for I := 0 to InstantiableNodes.Count - 1 do
  begin
    N := InstantiableNodes[I].Create;
    try
      AssertTrue(
        N.Functionality(TTransformFunctionality) = N.TransformFunctionality);

      { if a node has field with chTransform, it must support TTransformFunctionality.
        TCastleSceneCore.HandleChangeTransform assumes this. }
      if ContainsCHTransformField(N) then
      begin
        AssertTrue(N.Functionality(TTransformFunctionality) <> nil);
        AssertTrue(N.TransformFunctionality <> nil);
      end;

      { if, and only if, a node implements TTransformFunctionality, it must have
        TransformationChange = ntcTransform }
      AssertTrue(
        (N.Functionality(TTransformFunctionality) <> nil) =
        (N.TransformationChange = ntcTransform));
    except
      Writeln('TestTransformFunctionality failed for ', N.ClassName);
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

function LoadX3DClassicStream(const Stream: TStream): TX3DRootNode;
begin
  Result := LoadNode(Stream, '', 'model/x3d+vrml');
end;

function LoadX3DXmlStream(const Stream: TStream): TX3DRootNode;
begin
  Result := LoadNode(Stream, '', 'model/x3d+xml');
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
    Node := LoadX3DXmlStream(TempStream);

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
    Node := LoadX3DXmlStream(TempStream);
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
    Node := LoadX3DXmlStream(TempStream);
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
    Text: TTextNode;
    Shape: TShapeNode;
    Touch: TTouchSensorNode;
  begin
    AssertEquals(2, Node.FdChildren.Count);
    AssertTrue(Node.FdChildren[0] is TShapeNode);
    AssertTrue(Node.FdChildren[1] is TTouchSensorNode);

    Shape := Node.FdChildren[0] as TShapeNode;
    Touch := Node.FdChildren[1] as TTouchSensorNode;
    Text := Shape.Geometry as TTextNode;

    StringField := Text.FdString;
    AssertEquals(2, StringField.Count);
    AssertEquals(ValidString, StringField.Items[0]);
    AssertEquals(ValidString2, StringField.Items[1]);
    AssertEquals(ValidString, Touch.Description);
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

    //Writeln(StreamToString(TempStream)); // useful to debug

    TempStream.Position := 0;
    Node := LoadX3DClassicStream(TempStream);
    Assertions(Node);

    TempStream.Position := 0;
    TempStream.Size := 0;
    Save3D(Node, TempStream, '', '', xeXML, true);
    FreeAndNil(Node);

    //Writeln(StreamToString(TempStream)); // useful to debug

    TempStream.Position := 0;
    Node := LoadX3DXmlStream(TempStream);
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
    AssertFalse(IFS.FdSolid.Value);
    AssertFalse(IFS.SolidField.Value);
    AssertFalse(IFS.Solid);
  finally FreeAndNil(IFS) end;

  // LineSet doesn't have FdSolid field, but still Solid property should exist
  LineSet := TLineSetNode.Create;
  try
    //AssertTrue(LineSet.FdSolid.Value);
    AssertTrue(LineSet.SolidField = nil);
    AssertTrue(LineSet.Solid);

    LineSet.Solid := false;
    //AssertFalse(LineSet.FdSolid.Value);
    AssertTrue(LineSet.SolidField = nil);
    AssertFalse(LineSet.Solid);
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
    AssertFalse(IFS.FdConvex.Value);
    AssertFalse(IFS.ConvexField.Value);
    AssertFalse(IFS.Convex);
  finally FreeAndNil(IFS) end;

  // LineSet doesn't have FdConvex field, but still Convex property should exist
  LineSet := TLineSetNode.Create;
  try
    //AssertTrue(LineSet.FdConvex.Value);
    AssertTrue(LineSet.ConvexField = nil);
    AssertTrue(LineSet.Convex);

    LineSet.Convex := false;
    //AssertFalse(LineSet.FdConvex.Value);
    AssertTrue(LineSet.ConvexField = nil);
    AssertFalse(LineSet.Convex);
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

    '"castlescript:&#xA;function initialize(time)&#xA;  { set up first thunder in the future }&#xA;  '+
    'startLight1AndAudio :=  time +&#xA;    durationBetweenConst + random() * durationBetweenRandom1;&#xA;  startLight2 := startLight1AndAudio + ' +
    'blink2Start&#xA;&#xA;function forceThunderNow(value, time)&#xA;  when (value,&#xA;    startLight1AndAudio := time;&#xA;    startLight2 := '+
    'startLight1AndAudio + blink2Start)&#xA;&#xA;function light2Active(value, time)&#xA;  when (and(not(value), not(audioActive)),&#xA;    ' +
    '{ Once everything finished (2nd light blink and sound) finished,&#xA;      set up next thunder in the future.&#xA;      ' +
    'We can only do it once everything finished, as X3D spec says that&#xA;      &apos;Any set_startTime events to an active time-dependent node are ignored.&apos; }&#xA;    startLight1AndAudio := startLight1AndAudio +&#xA;      ' +
    'durationBetweenConst + random() * durationBetweenRandom2;&#xA;  ' +
    '  startLight2 := startLight1AndAudio + blink2Start)&#xA;&#xA;function ' +
    'audioActive(value, time)&#xA;  when (and(not(value), not(light2Active)),&#xA;    ' +
    '{ Exactly like light2Active. We have to watch for both light2Active&#xA;      and audioActive, as we don&apos;t know which one takes longer: light blinking&#xA;      or audio sound. }&#xA;    ' +
    'startLight1AndAudio := startLight1AndAudio +&#xA;      durationBetweenConst + random() * durationBetweenRandom2;&#xA;   ' +
    ' startLight2 := startLight1AndAudio + blink2Start)&#xA;"');
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

procedure TTestX3DNodes.WeakLinkUnusedWarning(const Category, S: string);
begin
  if Pos('GeneratedShadowMap.light', S) <> 0 then
    raise EWeakLinkUnused.Create('We want this warning, good: ' + S)
  else
    raise Exception.Create('Some invalid warning: ' + S);
end;

procedure TTestX3DNodes.TestWeakLinkUnusedWarning;
begin
  ApplicationProperties.OnWarning.Add({$ifdef FPC}@{$endif}WeakLinkUnusedWarning);
  try
    try
      LoadNode('castle-data:/warning_when_new_node_as_shadow_map_light.x3dv');
      raise Exception.Create('We should not get here, expected EWeakLinkUnused on the way');
    except
      on EWeakLinkUnused do ; { good, silence this for the sake of test }
    end;
  finally
    ApplicationProperties.OnWarning.Remove({$ifdef FPC}@{$endif}WeakLinkUnusedWarning);
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
    G.AddChildren([S, S], false);
    AssertEquals(1, G.FdChildren.Count);
    G.AddChildren([S, S, S], false);
    AssertEquals(1, G.FdChildren.Count);
    // only one instance of S is added

    S := TShapeNode.Create;
    G.AddChildren([S, S], false);
    AssertEquals(2, G.FdChildren.Count);

    S := TShapeNode.Create;
    G.AddChildren(S, false);
    AssertEquals(3, G.FdChildren.Count);
    G.RemoveChildren(S);
    AssertEquals(2, G.FdChildren.Count);

    FieldToSend := TMFNode.CreateUndefined(nil, false, 'temporary');
    try
      S := TShapeNode.Create;
      FieldToSend.Add(S);
      S := TShapeNode.Create;
      FieldToSend.Add(S);
      FieldToSend.Add(S); // will be ignored by AddChildren with AllowDuplicates=false
      FieldToSend.Add(S); // will be ignored by AddChildren with AllowDuplicates=false
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

procedure TTestX3DNodes.TestAddChildrenAllowDuplicates;
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
    AssertEquals(2, G.FdChildren.Count);
    G.AddChildren([S, S, S]);
    AssertEquals(5, G.FdChildren.Count);
    // multiple instances of S are added

    S := TShapeNode.Create;
    G.AddChildren([S, S]);
    AssertEquals(7, G.FdChildren.Count);

    S := TShapeNode.Create;
    G.AddChildren(S);
    AssertEquals(8, G.FdChildren.Count);
    G.RemoveChildren(S);
    AssertEquals(7, G.FdChildren.Count);

    FieldToSend := TMFNode.CreateUndefined(nil, false, 'temporary');
    try
      S := TShapeNode.Create;
      FieldToSend.Add(S);
      S := TShapeNode.Create;
      FieldToSend.Add(S);
      FieldToSend.Add(S); // will be ignored by AddChildren with AllowDuplicates=false
      FieldToSend.Add(S); // will be ignored by AddChildren with AllowDuplicates=false
      G.EventAddChildren.Send(FieldToSend, TX3DTime.Oldest);

      // adding through EventAddChildren works
      AssertEquals(9, G.FdChildren.Count);
    finally FreeAndNil(FieldToSend) end;

    FieldToSend := TMFNode.CreateUndefined(nil, false, 'temporary');
    try
      M := TMaterialNode.Create;
      FieldToSend.Add(M);
      G.EventAddChildren.Send(FieldToSend, TX3DTime.Oldest);

      // adding Material is ignored, not a child node
      AssertEquals(9, G.FdChildren.Count);
    finally FreeAndNil(FieldToSend) end;
  finally FreeAndNil(G) end;
end;

procedure TTestX3DNodes.TestMetadataArray;
var
  Transform: TTransformNode;
  MetadataString: TMetadataStringNode;
begin
  Transform := TTransformNode.Create;

  { Use properties
    MetadataBooleanArray / MetadataStringArray  / MetadataIntegerArray / MetadataDoubleArray.
    Think of each metadata, as a mapping from a unique key (string) ->
    to an array of booleans / strings / integers / doubles.

    Underneath, more complicated scenarios are possible, but if you only
    use these properties to get/set metadata, then it remains simple. }

  Assert(not Transform.MetadataBooleanArray['my_boolean_value', 0]); // not yet set, means it is false
  Transform.MetadataBooleanArray['my_boolean_value', 0] := true;
  Assert(Transform.MetadataBooleanArray['my_boolean_value', 0]);

  Assert(Transform.MetadataStringArray['my_string_value', 0] = ''); // not yet set, means it is ''
  Assert(Transform.MetadataStringArray['my_boolean_value', 0] = ''); // invalid type, means it is ''
  Transform.MetadataStringArray['my_string_value', 0] := 'apple';
  Transform.MetadataStringArray['my_string_value', 2] := 'banana';
  Assert((Transform.FindMetadata('my_string_value') as TMetadataStringNode).FdValue.Count = 3);
  Assert(Transform.MetadataStringArray['my_string_value', 0] = 'apple');
  Assert(Transform.MetadataStringArray['my_string_value', 1] = ''); // middle value is empty
  Assert(Transform.MetadataStringArray['my_string_value', 2] = 'banana');
  Assert(Transform.MetadataStringArray['my_string_value', 3] = ''); // not yet set, means it is ''

  Assert(Transform.MetadataIntegerArray['my_integer_value', 0] = 0); // not yet set, means it is ''
  Assert(Transform.MetadataIntegerArray['my_boolean_value', 0] = 0); // invalid type, means it is ''
  Transform.MetadataIntegerArray['my_integer_value', 2] := 123;
  Assert(Transform.MetadataIntegerArray['my_integer_value', 0] = 0);
  Assert(Transform.MetadataIntegerArray['my_integer_value', 1] = 0);
  Assert(Transform.MetadataIntegerArray['my_integer_value', 2] = 123);
  Assert(Transform.MetadataIntegerArray['my_integer_value', 1233] = 0);

  Assert(Transform.MetadataDoubleArray['my_double_value', 0] = 0.0); // not yet set, means it is 0.0
  Assert(Transform.MetadataDoubleArray['my_boolean_value', 0] = 0); // invalid type, means it is 0.0
  Transform.MetadataDoubleArray['my_double_value', 0] := 123.456;
  Assert(SameValue(Transform.MetadataDoubleArray['my_double_value', 0], 123.456));
  Assert(Transform.MetadataDoubleArray['my_double_value', 1233] = 0.0);

  { More manual way (not advised) }
  MetadataString := TMetadataStringNode.Create;
  MetadataString.NameField := 'fruit-type';
  MetadataString.SetValue(['banana']);
  (Transform.Metadata as TMetadataSetNode).FdValue.Add(MetadataString);

  Assert(Transform.Metadata is TMetadataSetNode);
  Assert((Transform.Metadata as TMetadataSetNode).FdValue.Count = 5);
  Assert((Transform.Metadata as TMetadataSetNode).FdValue.Items[0] is TMetadataBooleanNode);
  Assert((Transform.Metadata as TMetadataSetNode).FdValue.Items[1] is TMetadataStringNode);
  Assert((Transform.Metadata as TMetadataSetNode).FdValue.Items[2] is TMetadataIntegerNode);
  Assert((Transform.Metadata as TMetadataSetNode).FdValue.Items[3] is TMetadataDoubleNode);
  Assert((Transform.Metadata as TMetadataSetNode).FdValue.Items[4] = MetadataString);

  FreeAndNil(Transform);
end;

procedure TTestX3DNodes.TestMetadata;
var
  Transform: TTransformNode;
  MetadataString: TMetadataStringNode;
begin
  Transform := TTransformNode.Create;

  { Advised way: use properties
    MetadataBoolean / MetadataString  / MetadataInteger / MetadataDouble.
    Think of each metadata, as a mapping from a unique key (string) ->
    to an array of booleans / strings / integers / doubles.

    Underneath, more complicated scenarios are possible, but if you only
    use these properties to get/set metadata, then it remains simple. }

  Assert(not Transform.MetadataBoolean['my_boolean_value']); // not yet set, means it is false
  Transform.MetadataBoolean['my_boolean_value'] := true;
  Assert(Transform.MetadataBoolean['my_boolean_value']);

  Assert(Transform.MetadataString['my_string_value'] = ''); // not yet set, means it is ''
  Assert(Transform.MetadataString['my_boolean_value'] = ''); // invalid type, means it is ''
  Transform.MetadataString['my_string_value'] := 'apple';
  Transform.MetadataStringArray['my_string_value', 2] := 'banana';
  Assert((Transform.FindMetadata('my_string_value') as TMetadataStringNode).FdValue.Count = 3);
  Assert(Transform.MetadataString['my_string_value'] = 'apple');
  Assert(Transform.MetadataStringArray['my_string_value', 1] = ''); // middle value is empty
  Assert(Transform.MetadataStringArray['my_string_value', 2] = 'banana');
  Assert(Transform.MetadataStringArray['my_string_value', 3] = ''); // not yet set, means it is ''

  Assert(Transform.MetadataInteger['my_integer_value'] = 0); // not yet set, means it is ''
  Assert(Transform.MetadataInteger['my_boolean_value'] = 0); // invalid type, means it is ''
  Transform.MetadataIntegerArray['my_integer_value', 2] := 123;
  Assert(Transform.MetadataInteger['my_integer_value'] = 0);
  Assert(Transform.MetadataIntegerArray['my_integer_value', 1] = 0);
  Assert(Transform.MetadataIntegerArray['my_integer_value', 2] = 123);
  Assert(Transform.MetadataIntegerArray['my_integer_value', 1233] = 0);

  Assert(Transform.MetadataDouble['my_double_value'] = 0.0); // not yet set, means it is 0.0
  Assert(Transform.MetadataDouble['my_boolean_value'] = 0); // invalid type, means it is 0.0
  Transform.MetadataDouble['my_double_value'] := 123.456;
  Assert(SameValue(Transform.MetadataDouble['my_double_value'], 123.456));
  Assert(Transform.MetadataDoubleArray['my_double_value', 1233] = 0.0);

  { More manual way (not advised) }
  MetadataString := TMetadataStringNode.Create;
  MetadataString.NameField := 'fruit-type';
  MetadataString.SetValue(['banana']);
  (Transform.Metadata as TMetadataSetNode).FdValue.Add(MetadataString);

  Assert(Transform.Metadata is TMetadataSetNode);
  Assert((Transform.Metadata as TMetadataSetNode).FdValue.Count = 5);
  Assert((Transform.Metadata as TMetadataSetNode).FdValue.Items[0] is TMetadataBooleanNode);
  Assert((Transform.Metadata as TMetadataSetNode).FdValue.Items[1] is TMetadataStringNode);
  Assert((Transform.Metadata as TMetadataSetNode).FdValue.Items[2] is TMetadataIntegerNode);
  Assert((Transform.Metadata as TMetadataSetNode).FdValue.Items[3] is TMetadataDoubleNode);
  Assert((Transform.Metadata as TMetadataSetNode).FdValue.Items[4] = MetadataString);

  FreeAndNil(Transform);
end;

procedure TTestX3DNodes.TestNiceName;
var
  N: TX3DNode;
begin
  N := TX3DNode.Create;
  AssertEquals('TX3DNode', N.NiceName);
  FreeAndNil(N);

  N := TX3DRootNode.Create;
  AssertEquals('Group:TX3DRootNode', N.NiceName);
  FreeAndNil(N);

  N := TGroupNode.Create;
  AssertEquals('Group', N.NiceName);
  FreeAndNil(N);

  N := TX3DNode.Create;
  N.X3DName := 'Foo';
  AssertEquals('TX3DNode(Foo)', N.NiceName);
  FreeAndNil(N);

  N := TX3DRootNode.Create;
  N.X3DName := 'Foo';
  AssertEquals('Group:TX3DRootNode(Foo)', N.NiceName);
  FreeAndNil(N);

  N := TGroupNode.Create;
  N.X3DName := 'Foo';
  AssertEquals('Group(Foo)', N.NiceName);
  FreeAndNil(N);
end;

procedure TTestX3DNodes.TestTextureProperties;
var
  N: TTexturePropertiesNode;
  I: Integer;
begin
  N := TTexturePropertiesNode.Create;
  try
    for I := 0 to N.FieldsCount - 1 do
      if N.Fields[I].X3DName <> 'metadata' then
        AssertTrue(N.Fields[I].ExecuteChange = chTexturePropertiesNode);
  finally FreeAndNil(N) end;
end;

procedure TTestX3DNodes.TestFixNames;
var
  R: TX3DRootNode;
begin
  R := LoadNode('castle-data:/test_fix_names.x3dv');
  try
    { We no longer do renames, so this test outcome just checks that nothing changed }
    {
    AssertEquals('AA', R.FdChildren[0].X3DName);
    AssertEquals('AA_2', R.FdChildren[1].X3DName);
    AssertEquals('AA_3', R.FdChildren[2].X3DName);
    AssertEquals('AA_4', R.FdChildren[3].X3DName);
    AssertEquals('AA_5', R.FdChildren[4].X3DName);

    AssertEquals('BB_1', R.FdChildren[5].X3DName);
    AssertEquals('BB_2', R.FdChildren[6].X3DName);
    AssertEquals('BB_3', R.FdChildren[7].X3DName);
    AssertEquals('BB_4', R.FdChildren[8].X3DName);
    AssertEquals('BB_5', R.FdChildren[9].X3DName);

    AssertEquals('CC_1a1', R.FdChildren[10].X3DName);
    AssertEquals('CC_1a1_2', R.FdChildren[11].X3DName);
    AssertEquals('CC_1a1_3', R.FdChildren[12].X3DName);
    AssertEquals('CC_1a1_4', R.FdChildren[13].X3DName);
    AssertEquals('CC_1a1_5', R.FdChildren[14].X3DName);

    AssertEquals('DD_3', R.FdChildren[15].X3DName);
    AssertEquals('DD_4', R.FdChildren[16].X3DName);
    AssertEquals('DD_5', R.FdChildren[17].X3DName);
    AssertEquals('DD_6', R.FdChildren[18].X3DName);
    AssertEquals('DD_7', R.FdChildren[19].X3DName);

    AssertEquals('EE_3', R.FdChildren[20].X3DName);
    AssertEquals('EE_4', R.FdChildren[21].X3DName);
    AssertEquals('EE_5', R.FdChildren[22].X3DName);
    AssertEquals('EE_6', R.FdChildren[23].X3DName);
    AssertEquals('EE_7', R.FdChildren[24].X3DName);
    }
    AssertEquals('AA', R.FdChildren[0].X3DName);
    AssertEquals('AA', R.FdChildren[1].X3DName);
    AssertEquals('AA', R.FdChildren[2].X3DName);
    AssertEquals('AA', R.FdChildren[3].X3DName);
    AssertEquals('AA', R.FdChildren[4].X3DName);

    AssertEquals('BB_1', R.FdChildren[5].X3DName);
    AssertEquals('BB_1', R.FdChildren[6].X3DName);
    AssertEquals('BB_1', R.FdChildren[7].X3DName);
    AssertEquals('BB_1', R.FdChildren[8].X3DName);
    AssertEquals('BB_1', R.FdChildren[9].X3DName);

    AssertEquals('CC_1a1', R.FdChildren[10].X3DName);
    AssertEquals('CC_1a1', R.FdChildren[11].X3DName);
    AssertEquals('CC_1a1', R.FdChildren[12].X3DName);
    AssertEquals('CC_1a1', R.FdChildren[13].X3DName);
    AssertEquals('CC_1a1', R.FdChildren[14].X3DName);

    AssertEquals('DD_3', R.FdChildren[15].X3DName);
    AssertEquals('DD_3', R.FdChildren[16].X3DName);
    AssertEquals('DD_3', R.FdChildren[17].X3DName);
    AssertEquals('DD_3', R.FdChildren[18].X3DName);
    AssertEquals('DD_3', R.FdChildren[19].X3DName);

    AssertEquals('EE_3', R.FdChildren[20].X3DName);
    AssertEquals('EE_3', R.FdChildren[21].X3DName);
    AssertEquals('EE_4', R.FdChildren[22].X3DName);
    AssertEquals('EE_4', R.FdChildren[23].X3DName);
    AssertEquals('EE_5', R.FdChildren[24].X3DName);
  finally FreeAndNil(R) end;
end;

procedure TTestX3DNodes.TestAutomaticWeakLink;
var
  R: TX3DRootNode;
  MyTransform: TTransformNode;
  Script: TScriptNode;
begin
  R := LoadNode('castle-data:/script_node_recursive_def_use.x3dv');
  try
    AssertTrue(R.FdChildren.Count = 1);
    AssertTrue(R.FdChildren[0].X3DName = 'MyTransform');
    MyTransform := R.FdChildren.Items[0] as TTransformNode;

    AssertTrue(MyTransform.FdChildren.Count = 1);
    AssertTrue(MyTransform.FdChildren[0] is TScriptNode);
    Script := MyTransform.FdChildren[0] as TScriptNode;

    AssertTrue((Script.Field('container1', true) as TSFNode).Value = MyTransform);
    AssertTrue((Script.Field('container2', true) as TSFNode).Value = MyTransform);
    AssertTrue((Script.Field('container3', true) as TSFNode).Value =
               (Script.Field('container5', true) as TSFNode).Value);
    AssertTrue((Script.Field('container4', true) as TSFNode).Value =
               (Script.Field('container6', true) as TSFNode).Value);
  finally FreeAndNil(R) end;
end;

procedure TTestX3DNodes.TestNonGenericFind;
var
  Mat: TPhysicalMaterialNode;
  Appearance: TAppearanceNode;
  Shape: TShapeNode;
  Box: TBoxNode;
  Switch: TSwitchNode;
  RootNode: TX3DRootNode;
begin
  Mat := TPhysicalMaterialNode.Create('Foo');
  Mat.BaseColor := RedRGB;

  Appearance := TAppearanceNode.Create('Foo');
  Appearance.Material := Mat;

  Box := TBoxNode.CreateWithShape(Shape);
  Box.X3DName := 'Foo';
  Box.Size := Vector3(1, 2, 3);
  Shape.X3DName := 'Foo';
  Shape.Appearance := Appearance;

  Switch := TSwitchNode.Create;
  Switch.WhichChoice := -1; // Shape is inactive, but it doesn't matter for Find
  Switch.AddChildren(Shape);

  RootNode := TX3DRootNode.Create;
  try
    RootNode.AddChildren(Switch);

    AssertTrue(RootNode.FindNode(TX3DNode, 'Foo') <> nil); // undefined which node it will be

    AssertTrue((RootNode.FindNode(TPhysicalMaterialNode, 'Foo') as TPhysicalMaterialNode) <> nil);
    AssertVectorEquals(RedRGB, (RootNode.FindNode(TPhysicalMaterialNode, 'Foo') as TPhysicalMaterialNode).BaseColor);

    AssertTrue((RootNode.FindNode(TBoxNode, 'Foo') as TBoxNode) <> nil);
    AssertVectorEquals(Vector3(1, 2, 3), (RootNode.FindNode(TBoxNode, 'Foo') as TBoxNode).Size);
  finally FreeAndNil(RootNode) end;
end;

{$ifdef GENERIC_METHODS}

procedure TTestX3DNodes.TestGenericFind;
var
  Mat: TPhysicalMaterialNode;
  Appearance: TAppearanceNode;
  Shape: TShapeNode;
  Box: TBoxNode;
  Switch: TSwitchNode;
  RootNode: TX3DRootNode;
begin
  Mat := TPhysicalMaterialNode.Create('Foo');
  Mat.BaseColor := RedRGB;

  Appearance := TAppearanceNode.Create('Foo');
  Appearance.Material := Mat;

  Box := TBoxNode.CreateWithShape(Shape);
  Box.X3DName := 'Foo';
  Box.Size := Vector3(1, 2, 3);
  Shape.X3DName := 'Foo';
  Shape.Appearance := Appearance;

  Switch := TSwitchNode.Create;
  Switch.WhichChoice := -1; // Shape is inactive, but it doesn't matter for Find
  Switch.AddChildren(Shape);

  RootNode := TX3DRootNode.Create;
  try
    RootNode.AddChildren(Switch);

    AssertTrue(RootNode.{$ifdef FPC}specialize{$endif} Find<TX3DNode>('Foo') <> nil); // undefined which node it will be

    AssertTrue(RootNode.{$ifdef FPC}specialize{$endif} Find<TPhysicalMaterialNode>('Foo') <> nil);
    AssertVectorEquals(RedRGB, RootNode.{$ifdef FPC}specialize{$endif} Find<TPhysicalMaterialNode>('Foo').BaseColor);

    AssertTrue(RootNode.{$ifdef FPC}specialize{$endif} Find<TBoxNode>('Foo') <> nil);
    AssertVectorEquals(Vector3(1, 2, 3), RootNode.{$ifdef FPC}specialize{$endif} Find<TBoxNode>('Foo').Size);
  finally FreeAndNil(RootNode) end;
end;

{$endif GENERIC_METHODS}

procedure TTestX3DNodes.TestProtoExpansion;
var
  RootNode: TX3DRootNode;

  procedure CheckProtoInstance(const ProtoIndex: Integer; const CorrectPaletteUrl: String);
  var
    T: TTransformNode;
    S: TShapeNode;
    CS: TComposedShaderNode;
    PalleteField: TSFNode;
    IT: TImageTextureNode;
  begin
    AssertTrue(RootNode.FdChildren[ProtoIndex] is TTransformNode);
    T := RootNode.FdChildren[ProtoIndex] as TTransformNode;
    AssertEquals(1, T.FdChildren.Count);
    AssertTrue(T.FdChildren[0] is TShapeNode);
    S := T.FdChildren[0] as TShapeNode;
    AssertEquals(1, S.Appearance.FdShaders.Count);
    AssertTrue(S.Appearance.FdShaders[0] is TComposedShaderNode);
    CS := S.Appearance.FdShaders[0] as TComposedShaderNode;
    PalleteField := CS.Field('palette') as TSFNode;
    AssertTrue(PalleteField <> nil);
    AssertTrue(PalleteField.Value is TImageTextureNode);
    IT := PalleteField.Value as TImageTextureNode;
    AssertEquals(1, IT.FdUrl.Count);
    AssertEquals(CorrectPaletteUrl, IT.FdUrl.Items[0]);
  end;

begin
  RootNode := LoadNode('castle-data:/proto_copy_test/model.x3dv');
  try
    AssertEquals(2, RootNode.FdChildren.Count);
    AssertTrue(RootNode.FdChildren[1] is TTransformNode);
    CheckProtoInstance(0, 'palette1.png');
    CheckProtoInstance(1, 'palette2.png');
  finally FreeAndNil(RootNode) end;
end;

procedure TTestX3DNodes.TestSolidField;
var
  I, J: Integer;
  N: TAbstractGeometryNode;
begin
  for I := 0 to InstantiableNodes.Count - 1 do
  begin
    if InstantiableNodes[I].InheritsFrom(TAbstractGeometryNode) then
    begin
      N := InstantiableNodes[I].Create as TAbstractGeometryNode;
      try
        for J := 0 to N.FieldsCount - 1 do
          if N.Fields[J].X3DName = 'solid' then
            AssertTrue('SolidField overridden correctly for ' + N.X3DType, N.SolidField = N.Fields[J]);
      finally FreeAndNil(N) end;
    end;
  end;
end;

initialization
  RegisterTest(TTestX3DNodes);
end.
