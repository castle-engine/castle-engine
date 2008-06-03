{
  Copyright 2004-2007 Michalis Kamburelis.

  This file is part of test_kambi_units.

  test_kambi_units is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  test_kambi_units is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with test_kambi_units; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit TestVRMLNodes;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestVRMLNodes = class(TTestCase)
    procedure TestNodesManager;

    { This is really large test that reads and writes various VRML files
      and checks whether the generated VRML file is the same.
      It checks "the same" by looking comparing sequence of VRMLLexer
      tokens for them.

      So this checks both reading and writing of VRML files.

      Deactivated... it can't check like this, lexer tokens may be
      different due to the order of fields.
    }
    { procedure TestParseSaveToFile; } { }

    procedure TestUniqueFields;

    procedure TestInterfaceSupports;

    procedure TestAllowedChildren;

    procedure TestContainerField;
  end;

implementation

uses KambiUtils, VRMLNodes, VRMLLexer, KambiClassUtils, KambiFilesUtils;

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

(*
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
        '"{"', '"}"', '"["', '"]"', '"("', '")"', '"|"', '","', '"."',
        'float', 'integer', 'string', 'end of file');
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
      ( (T1.Token <> vtName) or (T1.Name = T2.Name) ) and
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
      SaveToVRMLFile(Node, NewFile, '', false);

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
  TestReadWrite('../../kambi_vrml_test_suite/vrml_2/proto_nested.wrl');
end;
*)

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
    TNodeMatrixTransform, TNodeRotation,
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

procedure TTestVRMLNodes.TestContainerField;
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

initialization
 RegisterTest(TTestVRMLNodes);
end.
