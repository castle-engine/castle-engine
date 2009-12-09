{ -*- compile-command: "./compile_console.sh" -*- }
{
  Copyright 2004-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit TestVRMLNodes;

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
    procedure TestGeometryNodesImplemented;
    procedure TestDestructionNotification;

  private
    procedure DummyTriangleProc(const Tri: TTriangle3Single;
      State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
      const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
  end;

implementation

uses KambiUtils, VRMLLexer, KambiClassUtils, KambiFilesUtils;

{ TNode* ------------------------------------------------------------ }

type
  TNodeSpecial = class(TVRMLNode)
    function NodeTypeName: string; override;
  end;

function TNodeSpecial.NodeTypeName: string;
begin
 result := 'OohImSoSpecial';
end;

type
  TNodeSomething = class(TVRMLNode)
    class function ClassNodeTypeName: string; override;
  end;

class function TNodeSomething.ClassNodeTypeName: string;
begin
 result := 'WellImNothingSpecial';
end;

{ ------------------------------------------------------------ }

procedure TTestVRMLNodes.TestNodesManager;
begin
 try
  { throw exception because TNodeSpecial.ClassNodeTypeName = '' }
  NodesManager.RegisterNodeClass(TNodeSpecial);
  raise Exception.Create('NodesManager.RegisterNodeClass(TNodeSpecial); SHOULD throw exception');
 except on ENodesManagerError do ; end;

 try
  { throw exception because TNodeFog is already registered }
  NodesManager.RegisterNodeClass(TNodeFog);
  raise Exception.Create('NodesManager.RegisterNodeClass(TNodeFog); SHOULD throw exception');
 except on ENodesManagerError do ; end;

 try
  { this should succeed }
  NodesManager.RegisterNodeClass(TNodeSomething);
 finally
  NodesManager.UnRegisterNodeClass(TNodeSomething);
 end;
end;

{ TVRMLTokenInfo and TDynVRMLTokenInfoArray ---------------------------------- }

type
  TVRMLTokenInfo = record
    Token: TVRMLToken;
    Float: Float; // for both vtFloat and vtInteger
    Name: string; // for vtName
    AString: string; // for vtString
    case TVRMLToken of
      vtKeyword: (Keyword: TVRMLKeyword);
      vtInteger: (Integer: Int64);
  end;
  PVRMLTokenInfo = ^TVRMLTokenInfo;

  TDynArrayItem_1 = TVRMLTokenInfo;
  PDynArrayItem_1 = PVRMLTokenInfo;
  {$define DYNARRAY_1_IS_STRUCT}
  {$define DYNARRAY_1_IS_INIT_FINI_TYPE}
  {$define read_interface}
  {$define read_implementation}
  {$I dynarray_1.inc}

type
  TDynVRMLTokenInfoArray = class(TDynArray_1)
    procedure AssertEqual(SecondValue: TDynVRMLTokenInfoArray);
    procedure ReadFromFile(const FileName: string);
  end;

procedure TDynVRMLTokenInfoArray.AssertEqual(
  SecondValue: TDynVRMLTokenInfoArray);

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
    'TDynVRMLTokenInfoArray.Equal: different counts %d and %d',
    [Count, SecondValue.Count]));
  for I := 0 to Count - 1 do
    AssertEqualTokens(Items[I], SecondValue.Items[I]);
end;

{ Note that this can be used to test correctly only files that can
  be correctly parsed by pure Lexer.NextToken calls. All valid VRML >= 2.0
  files are like that, although parser in practice has to use NextTokenForceXxx
  methods because of unfortunately
  1. invalid VRML files (that use some funny node names)
  2. VRML 1.0 ugly feature that string doesn't have to be enclosed in "" }
procedure TDynVRMLTokenInfoArray.ReadFromFile(const FileName: string);
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
    AppendItem(CurrentToken);
    while Lexer.Token <> vtEnd do
    begin
      Lexer.NextToken;
      AppendItem(CurrentToken);
    end;
  finally FreeAndNil(Lexer); end;
end;

procedure TTestVRMLNodes.TestParseSaveToFile;

  procedure TestReadWrite(const FileName: string);
  var
    First, Second: TDynVRMLTokenInfoArray;
    Node: TVRMLNode;
    S: TMemoryStream;
    SPeek: TPeekCharStream;
    NewFile: string;
  begin
    First := nil;
    Second := nil;
    Node := nil;
    try
      First := TDynVRMLTokenInfoArray.Create;
      First.ReadFromFile(FileName);

      Node := ParseVRMLFile(FileName, false);
      NewFile := GetTempPath + 'test_kambi_vrml_game_engine.wrl';
      SaveToVRMLFile(Node, NewFile, '');

      Second := TDynVRMLTokenInfoArray.Create;
      Second.ReadFromFile(NewFile);

      First.AssertEqual(Second);
    finally
      FreeAndNil(First);
      FreeAndNil(Second);
      FreeAndNil(Node);
    end;
  end;

begin
  TestReadWrite('../../kambi_vrml_test_suite/x3d/proto_sfnode_default.x3dv');
  TestReadWrite('../../kambi_vrml_test_suite/x3d/tricky_def_use.x3dv');
end;

procedure TTestVRMLNodes.TestInterfaceSupports;
var
  L: TVRMLNodeClassesList;

  function IndexOfAnyAncestorByClass(C: TVRMLNodeClass): boolean;
  var
    N: TVRMLNode;
  begin
    N := C.Create('', '');
    try
      Result := L.IndexOfAnyAncestor(N) <> -1;
    finally FreeAndNil(N) end;
  end;

begin
  { When our interfaces have appropriate GUIDs, "Supports" works Ok. }

  Assert(Supports(TNodeGroup_2, INodeX3DChildNode));
  Assert(Supports(TNodeSwitch_2, INodeX3DChildNode));
  Assert(not Supports(TNodeCone_2, INodeX3DChildNode));
  Assert(not Supports(TNodeAppearance, INodeX3DChildNode));
  Assert(not Supports(TVRMLNode, INodeX3DChildNode));
  Assert(not Supports(TObject, INodeX3DChildNode));

  L := TVRMLNodeClassesList.Create;
  try
    L.AddRegisteredImplementing(INodeX3DChildNode);
    { similar to above tests, but now using L.IndexOfAnyAncestor.
      So we test IndexOfAnyAncestor and AddRegisteredImplementing,
      AddRegisteredImplementing also uses "Supports" under the hood
      and results should be the same. }
    Assert(IndexOfAnyAncestorByClass(TNodeGroup_2));
    Assert(IndexOfAnyAncestorByClass(TNodeSwitch_2));
    Assert(not IndexOfAnyAncestorByClass(TNodeCone_2));
    Assert(not IndexOfAnyAncestorByClass(TNodeAppearance));
    Assert(not IndexOfAnyAncestorByClass(TVRMLNode));
  finally FreeAndNil(L) end;
end;

procedure TTestVRMLNodes.TestUniqueFields;
var
  I, J, K: Integer;
  N: TVRMLNode;
  CurrentName: string;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create('', '');
    try

      { Writeln(N.NodeTypeName, ' ', Supports(N, INodeX3DChildNode)); }

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
  AllowedChildrenNodes: TVRMLNodeClassesList;
  AllowedGeometryNodes: TVRMLNodeClassesList;
  I: Integer;
  N: TVRMLNode;
begin
  { AllowedChildrenNodes and AllowedGeometryNodes were written before X3D
    transition. I didn't then check AllowedChildren using some general
    inheritance classes, like X3D, but I had simply long lists for
    some properties.

    They were removed from VRMLNodes, since using X3D inheritance
    is obiously much simpler and long-term solution. For example,
    all children nodes simply inherit from TNodeX3DChildNode
    (actually, INodeX3DChildNode, and since FPC "Supports" doesn't work
    we simply have INodeX3DChildNode_Descendants lists... still, it's much
    shorter list than AllowedChildrenNodes).
    This avoids the need to maintain long lists like these below, that would be
    nightmare considering large number of X3D nodes.

    Still, since I already wrote these lists, they are used below
    for testing. To make sure X3D inheritance didn't break any
    previous behavior, all classes on AllowedChildrenNodes must inherit
    from TNodeX3DChildNode, and similat for geometry nodes. }

  AllowedChildrenNodes := TVRMLNodeClassesList.Create;
  AllowedChildrenNodes.AssignArray([
    { We add all nodes for VRML < 2.0, because we allow
      to mix VRML 1.0 inside VRML 2.0. }

    { Inventor spec nodes }
    TNodeIndexedTriangleMesh_1, TNodeRotationXYZ,

    { VRML 1.0 spec nodes }
    TNodeAsciiText_1, TNodeCone_1, TNodeCube_1, TNodeCylinder_1,
    TNodeIndexedFaceSet_1, TNodeIndexedLineSet_1,
    TNodePointSet_1, TNodeSphere_1,
    TNodeCoordinate3, TNodeFontStyle_1, TNodeInfo, TNodeLOD_1, TNodeMaterial_1,

    { TNodeNormal used to also be allowed here, but it's also used by X3D,
      and I don't want to mess X3D inheritance by making TNodeNormal descendant
      of INodeX3DChildNode --- which is required only when you mix VRML 1.0
      and X3D... When mixing VRML 1.0 and VRML >= 2.0 in a singe file,
      you will have to live with warnings about Normal not allowed as
      children in VRML >= 2.0 nodes.

      Also TNodeTexture2 was here, but is removed: I don't want to mess
      X3D inheritance just for VRML 1.0 + 2.0 mixing feature. }

    TNodeMaterialBinding, TNodeNormalBinding,
    TNodeTexture2Transform,
    TNodeTextureCoordinate2, TNodeShapeHints,
    TNodeMatrixTransform_1, TNodeRotation,
    TNodeScale, TNodeTransform_1,
    TNodeTranslation,
    TNodeOrthographicCamera, TNodePerspectiveCamera,
    TNodeDirectionalLight_1, TNodePointLight_1, TNodeSpotLight_1,
    TNodeGroup_1, TNodeSeparator, TNodeSwitch_1, TNodeTransformSeparator,
    TNodeWWWAnchor,
    TNodeWWWInline,

    { Kambi non-standard nodes }
    TNodeKambiTriangulation,
    TNodeKambiHeadLight,
    //TNodeText3D,
    //TNodeBlendMode,
    //TNodeKambiAppearance,

    { VRML 2.0 spec nodes }
    TNodeAnchor,
    //TNodeAppearance,
    //TNodeAudioClip,
    TNodeBackground,
    TNodeBillboard,
    //TNodeBox,
    TNodeCollision,
    //TNodeColor,
    TNodeColorInterpolator,
    //TNodeCone_2,
    //TNodeContour2D,
    //TNodeCoordinate,
    { VRML 2.0 spec section "4.6.5 Grouping and children nodes"
      doesn't say is CoordinateDeformer allowed or not as children node.
      To be fixed when I'll implement CoordinateDeformer handling. }
    TNodeCoordinateDeformer,
    TNodeCoordinateInterpolator,
    //TNodeCylinder_2,
    TNodeCylinderSensor,
    TNodeDirectionalLight_2,
    //TNodeElevationGrid,
    //TNodeExtrusion,
    TNodeFog,
    { VRML 2.0 spec section "4.6.5 Grouping and children nodes"
      doesn't say is TNodeFontStyle allowed as children node,
      but FontStyle docs say that it's only for Text.fontStyle. }
    //TNodeFontStyle_2,
    //TNodeGeoCoordinate,
    //TNodeGeoElevationGrid,
    TNodeGeoLocation,
    TNodeGeoLOD,
    TNodeGeoMetadata,
    //TNodeGeoOrigin,
    TNodeGeoPositionInterpolator,
    TNodeGeoTouchSensor,
    TNodeGeoViewpoint,
    TNodeGroup_2,
    //TNodeImageTexture,
    //TNodeIndexedFaceSet_2,
    //TNodeIndexedLineSet_2,
    TNodeInline,
    { VRML 2.0 spec doesn't say InlineLoadControl is valid children
      node, it also doesn't say it's not valid. Common sense says
      it's valid. }
    TNodeInlineLoadControl,
    TNodeLOD_2,
    //TNodeMaterial_2,
    //TNodeMovieTexture,
    TNodeNavigationInfo,
    { Normal node is not a valid children node for VRML 2.0.
      But we don't have separate TNodeNormal_1 and TNodeNormal_2 classes,
      so node normal was already added here as all other VRML 1.0 nodes.
      So it's allowed children node for us --- in the spirit thst
      we allow to mix VRML 1.0 and 2.0. }
    //{ TNodeNormal, - registered already as VRML 1.0 node }
    TNodeNormalInterpolator,
    //TNodeNurbsCurve,
    //TNodeNurbsCurve2D,
    { VRML 2.0 spec section "4.6.5 Grouping and children nodes"
      doesn't say is NurbsGroup allowed or not as children node.
      To be fixed when I'll implement NurbsGroup handling. }
    TNodeNurbsGroup,
    TNodeNurbsPositionInterpolator_2,
    //TNodeNurbsSurface,
    //TNodeNurbsTextureSurface,
    TNodeOrientationInterpolator,
    { VRML 2.0 spec section "4.6.5 Grouping and children nodes"
      doesn't say is PixelTexture allowed or not as children node.
      But common sense says it's only for Appearance.texture field. }
    //TNodePixelTexture,
    TNodePlaneSensor,
    TNodePointLight_2,
    //TNodePointSet_2,
    //TNodePolyline2D,
    TNodePositionInterpolator,
    TNodeProximitySensor,
    TNodeScalarInterpolator,
    TNodeScript,
    TNodeShape,
    TNodeSound,
    //TNodeSphere_2,
    TNodeSphereSensor,
    TNodeSpotLight_2,
    TNodeSwitch_2,
    //TNodeText,
    //TNodeTextureCoordinate,
    //TNodeTextureTransform,
    TNodeTimeSensor,
    TNodeTouchSensor,
    TNodeTransform_2,
    //TNodeTrimmedSurface,
    TNodeViewpoint,
    TNodeVisibilitySensor,
    TNodeWorldInfo,

    { X3D nodes }
    //TNodeComposedShader,
    //TNodePackagedShader,
    //TNodeProgramShader,
    //TNodeShaderPart,
    //TNodeShaderProgram
    TNodeSwitch_2,
    TNodeLOD_2
  ]);

  AllowedGeometryNodes := TVRMLNodeClassesList.Create;
  AllowedGeometryNodes.AssignArray([
    TNodeBox,
    TNodeCone_2,
    TNodeContour2D,
    TNodeCylinder_2,
    TNodeElevationGrid,
    TNodeExtrusion,
    TNodeGeoElevationGrid,
    TNodeIndexedFaceSet_2,
    TNodeIndexedLineSet_2,
    TNodeNurbsCurve_2,
    TNodeNurbsSurface,
    TNodePointSet_2,
    TNodeSphere_2,
    TNodeText,
    TNodeText3D,
    TNodeTrimmedSurface
  ]);

  try
    for I := 0 to AllowedChildrenNodes.Count - 1 do
    try
      Assert(Supports(AllowedChildrenNodes[I], INodeX3DChildNode));

      { Just to make sure, check also the created class
        (I don't trust FPC interfaces for now...) }
      N := AllowedChildrenNodes[I].Create('', '');
      try
        Assert(Supports(N, INodeX3DChildNode));
      finally FreeAndNil(N) end;
    except
      on E: Exception do
      begin
        Writeln('Failed on ', AllowedChildrenNodes[I].ClassName, ' is INodeX3DChildNode');
        raise;
      end;
    end;

    for I := 0 to AllowedGeometryNodes.Count - 1 do
    try
      Assert(AllowedGeometryNodes[I].InheritsFrom(TNodeX3DGeometryNode));
    except
      on E: Exception do
      begin
        Writeln('Failed on ', AllowedGeometryNodes[I].ClassName, ' is TNodeX3DGeometryNode');
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
    'AudioClip=children' + NL +
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
  N: TVRMLNode;
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
           (not (N is TNodeFontStyle_1)) and
           (not (N is TNodeMaterial_1)) then
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
  N: TVRMLNode;
begin
  for I := 0 to NodesManager.RegisteredCount - 1 do
  begin
    N := NodesManager.Registered[I].Create('', '');
    try
      if (N is TVRMLGeometryNode) and
         { TNodeContour2D is an exception, see TNodeContour2D comments.
           It should be treated as non-geometry node for X3D.
           Fortunately, containerField is used only for X3D. }
         (not (N is TNodeContour2D)) then
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

procedure TTestVRMLNodes.DummyTriangleProc(const Tri: TTriangle3Single;
  State: TVRMLGraphTraverseState; GeometryNode: TVRMLGeometryNode;
  const MatNum, FaceCoordIndexBegin, FaceCoordIndexEnd: integer);
begin
end;

procedure TTestVRMLNodes.TestGeometryNodesImplemented;
var
  I: Integer;
  N: TVRMLNode;
  State: TVRMLGraphTraverseState;
begin
  State := TVRMLGraphTraverseState.Create;
  try
    for I := 0 to NodesManager.RegisteredCount - 1 do
    begin
      N := NodesManager.Registered[I].Create('', '');
      try
        if N is TVRMLGeometryNode then
        try
          { just test that abstract methods are overriden, and don't crash }
          (N as TVRMLGeometryNode).BoundingBox(State);
          (N as TVRMLGeometryNode).LocalBoundingBox(State);
          (N as TVRMLGeometryNode).VerticesCount(State, true);
          (N as TVRMLGeometryNode).VerticesCount(State, false);
          (N as TVRMLGeometryNode).TrianglesCount(State, true);
          (N as TVRMLGeometryNode).TrianglesCount(State, false);
// this is known to be not implemented for some nodes
//          (N as TVRMLGeometryNode).Triangulate(State, true, @DummyTriangleProc);
          (N as TVRMLGeometryNode).Triangulate(State, false, @DummyTriangleProc);
// this is known to be not implemented for some nodes
//          (N as TVRMLGeometryNode).LocalTriangulate(State, true, @DummyTriangleProc);
          (N as TVRMLGeometryNode).LocalTriangulate(State, false, @DummyTriangleProc);
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

type
  TMyObject = class
    procedure Foo(Node: TVRMLNode);
  end;

procedure TMyObject.Foo(Node: TVRMLNode);
begin
end;

procedure TTestVRMLNodes.TestDestructionNotification;
var
  A: TDynNodeDestructionNotificationArray;
  M1, M2, M3: TMyObject;
begin
  A := TDynNodeDestructionNotificationArray.Create;
  M1 := TMyObject.Create;
  M2 := TMyObject.Create;
  M3 := TMyObject.Create;
  try
    A.AppendItem(@M1.Foo);
    A.AppendItem(@M2.Foo);
    A.AppendItem(@M3.Foo);
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

initialization
 RegisterTest(TTestVRMLNodes);
end.
