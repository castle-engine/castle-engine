{
  Copyright 2018-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Load 3D models in the glTF 2.0 format (@link(LoadGltf)). }
unit X3DLoadInternalGltf;

{$I castleconf.inc}

interface

uses Classes,
  CastleUtils, CastleVectors, X3DNodes, X3DFields;

{ Load 3D model in the Gltf format, converting it to an X3D nodes graph.
  This routine is internally used by the @link(LoadNode) to load an Gltf file. }
function LoadGltf(const Stream: TStream; const BaseUrl: String): TX3DRootNode;

implementation

uses SysUtils, TypInfo, Math, PasGLTF, PasJSON, Generics.Collections,
  CastleClassUtils, CastleDownload, CastleURIUtils, CastleLog,
  CastleStringUtils, CastleTextureImages, CastleQuaternions,
  CastleImages, CastleVideos, CastleTimeUtils, CastleTransform,
  CastleLoadGltf, X3DLoadInternalUtils, CastleBoxes, CastleColors,
  CastleRenderOptions;

{$ifndef FPC}
{$POINTERMATH ON}
{$endif}

{ This unit implements reading glTF into X3D.
  We're using PasGLTF from Bero: https://github.com/BeRo1985/pasgltf/

  Docs:

  - To understand glTF, and PasGLTF API, see the glTF specification:
    https://github.com/KhronosGroup/glTF/tree/master/specification/2.0

  - This unit converts glTF to an X3D scene graph,
    so you should be familiar with X3D as well:
    https://castle-engine.io/vrml_x3d.php .

  - See https://castle-engine.io/creating_data_model_formats.php .

  TODOs:

  - In the future, we would like to avoid using
    Accessor.DecodeAsXxx. Instead we should load binary data straight to GPU,
    looking at buffers, already exposed by PasGLTF.
    New X3D node, like BufferGeometry (same as X3DOM) will need to be
    invented for this, and CastleInternalGeometryArrays will need to be rearranged.

  - Morph targets, or their animations, are not supported yet.

  - Skin animation is done by calculating CoordinateInterpolator at loading.
    At runtime we merely animate using CoordinateInterpolator.
    While this is simple, it has disadvantages:

    - Loading time is longer, as we need to calculate CoordinateInterpolator.
    - At runtime, interpolation and animation blending can only interpolate
      sets of positions (not bones) so:
      - They are not as accurate,
      - CoordinateInterpolator interpolation is done on CPU,
        it processes all mesh positions each frame.
        This is unoptimal, as glTF skinning can be done on GPU, with much smaller runtime cost.

  - See https://castle-engine.io/roadmap .
}

{ Convert simple types ------------------------------------------------------- }

function Vector3FromGltf(const V: TPasGLTF.TVector3): TVector3;
begin
  // as it happens, both structures have the same memory layout, so copy by a fast Move
  Assert(SizeOf(V) = SizeOf(Result));
  Move(V, Result, SizeOf(Result));
end;

function Vector4FromGltf(const V: TPasGLTF.TVector4): TVector4;
begin
  // as it happens, both structures have the same memory layout, so copy by a fast Move
  Assert(SizeOf(V) = SizeOf(Result));
  Move(V, Result, SizeOf(Result));
end;

function Matrix4FromGltf(const M: TPasGLTF.TMatrix4x4): TMatrix4;
begin
  // as it happens, both structures have the same memory layout, so copy by a fast Move
  Assert(SizeOf(M) = SizeOf(Result));
  Move(M, Result, SizeOf(Result));
end;

{ Convert glTF rotation (quaternion) to X3D (axis-angle). }
function RotationFromGltf(const V: TPasGLTF.TVector4): TVector4;
var
  RotationQuaternion: TQuaternion;
begin
  RotationQuaternion.Data.Vector4 := Vector4FromGltf(V);
  Result := RotationQuaternion.ToAxisAngle;
end;

{ TMyGltfDocument ------------------------------------------------------------ }

type
  { Descendant of TPasGLTF.TDocument that changes URI loading,
    to load URI using our CastleDownload, thus supporting all our URLs. }
  TMyGltfDocument = class(TPasGLTF.TDocument)
  strict private
    function CastleGetUri(const aURI: TPasGLTFUTF8String): TStream;
  public
    constructor Create(const Stream: TStream; const BaseUrl: String); reintroduce;
  end;

constructor TMyGltfDocument.Create(const Stream: TStream; const BaseUrl: String);
begin
  inherited Create;

  { The interpretation of RootPath lies on our side, in GetUri implementation.
    Just use it to store BaseUrl then. }
  RootPath := BaseUrl;
  GetURI := {$ifdef FPC}@{$endif}CastleGetUri;

  LoadFromStream(Stream);
end;

function TMyGltfDocument.CastleGetUri(const aURI: TPasGLTFUTF8String): TStream;
begin
  { Resolve and open URI using our CGE functions.
    Without this, TPasGLTF.TDocument.DefaultGetURI would always use TFileStream.Create,
    and not work e.g. with Android assets. }
  Result := Download(CombineURI(RootPath, aURI));
end;

{ TGltfAppearanceNode -------------------------------------------------------- }

type
  { X3D Appearance node extended to carry some additional information specified
    in glTF materials. }
  TGltfAppearanceNode = class(TAppearanceNode)
  public
    DoubleSided: Boolean;
    { Some glTF geometry is using this.
      Deliberately defined such that default @false is OK,
      and during import it may be set to @true. }
    Used: Boolean;
    { Some glTF geometry that is possibly lit is using this.
      All geometries are "lit" except lines and points without normals,
      see https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#point-and-line-materials .
      Deliberately defined such that default @false is OK,
      and during import it may be set to @true. }
    UsedAsLit: Boolean;
  end;

{ TSkinToInitialize ---------------------------------------------------------- }

type
  { Information about skin, to be used later. }
  TSkinToInitialize = class
    Skin: TPasGLTF.TSkin;
    { Direct children of this grouping node that are TShapeNode should have skinning applied. }
    Shapes: TAbstractGroupingNode;
    { Immediate parent of the Shapes node (it always has only one parent). }
    ShapesParent: TAbstractGroupingNode;
  end;

  TSkinToInitializeList = {$ifdef FPC}specialize{$endif} TObjectList<TSkinToInitialize>;

{ TAnimation ----------------------------------------------------------------- }

type
  // Which TTransformNode field is animated
  TGltfSamplerPath = (
    gsTranslation,
    gsRotation,
    gsScale
  );

  TInterpolator = record
    Node: TAbstractInterpolatorNode;
    Target: TTransformNode;
    Path: TGltfSamplerPath;
  end;

  TInterpolatorList = {$ifdef FPC}specialize{$endif} TList<TInterpolator>;

  { Information about created animation. }
  TAnimation = class
    TimeSensor: TTimeSensorNode;
    Interpolators: TInterpolatorList; //< Only TTransformNode instances
    constructor Create;
    destructor Destroy; override;
  end;

  TAnimationList = {$ifdef FPC}specialize{$endif} TObjectList<TAnimation>;

constructor TAnimation.Create;
begin
  inherited;
  Interpolators := TInterpolatorList.Create;
end;

destructor TAnimation.Destroy;
begin
  FreeAndNil(Interpolators);
  inherited;
end;

{ TAnimationSampler --------------------------------------------------------------- }

type
  TAnimationSampler = class
  strict private
    { Internal in SetTime. }
    CurrentTranslation: TVector3List;
    CurrentRotation: TVector4List;
    CurrentScale: TVector3List;
  public
    { Set this before @link(SetTime).
      List of TTransformNode nodes, ordered just list glTF nodes.
      Only initialized (non-nil and enough Count) for nodes that we created in ReadNode. }
    TransformNodes: TX3DNodeList;
    { Set this before @link(SetTime).
      Current animation applied by @link(SetTime). }
    Animation: TAnimation;
    { Owned by this object, calculated by @link(SetTime).
      Has the same size as TransformNodes, contains accumulated transformation matrix
      for each node.
      Contains undefined value for nodes that are @nil. }
    Transformations: TTransformationList;
    TransformNodesRoots: TPasGLTF.TScene.TNodes;
    TransformNodesGltf: TPasGLTF.TNodes;
    constructor Create;
    destructor Destroy; override;
    procedure SetTime(const Time: TFloatTime);
  end;

constructor TAnimationSampler.Create;
begin
  inherited;
  Transformations := TTransformationList.Create;
  CurrentTranslation := TVector3List.Create;
  CurrentRotation := TVector4List.Create;
  CurrentScale := TVector3List.Create;
end;

destructor TAnimationSampler.Destroy;
begin
  FreeAndNil(Transformations);
  FreeAndNil(CurrentTranslation);
  FreeAndNil(CurrentRotation);
  FreeAndNil(CurrentScale);
  inherited;
end;

procedure TAnimationSampler.SetTime(const Time: TFloatTime);

{ The implementation of this somewhat duplicates the logic
  of the animation and transformation at runtime,
  done by TTransformNode in CastleShapes, CastleSceneCore units.
  At one point I considered just using TTimeSensor.FakeTime
  or even TCastleSceneCore.ForceAnimationPose to set scene
  to given state in each SetTime, and then read resulting transformations
  from Scene.Shapes.

  However, this causes new complications:
  It would modify the nodes hierarchy, which means we should save/restore it.

  And it's not really much simpler, since the transformation hierarchy is quite simple.
}

  { Set all CurrentXxx values to reflect initial transformations of TransformNodes }
  procedure ResetCurrentTransformation;
  var
    I: Integer;
    Transform: TTransformNode;
  begin
    // initialize CurrentXxx lists
    for I := 0 to TransformNodes.Count - 1 do
      if TransformNodes[I] <> nil then
      begin
        Transform := TransformNodes[I] as TTransformNode;
        CurrentTranslation[I] := Transform.FdTranslation.Value;
        CurrentRotation   [I] := Transform.FdRotation   .Value;
        CurrentScale      [I] := Transform.FdScale      .Value;
      end;
  end;

  { Perform PositionInterpolator interpolation.
    Range and T are like results of KeyRange call. }
  function InterpolatePosition(const Interpolator: TPositionInterpolatorNode;
    const Range: Integer; const T: Single): TVector3;
  var
    KeyValue: TVector3List;
  begin
    KeyValue := Interpolator.FdKeyValue.Items;
    if Range = 0 then
      Result := KeyValue[0]
    else
    if Range = KeyValue.Count then
      Result := KeyValue[KeyValue.Count - 1]
    else
      Result := TVector3.Lerp(T, KeyValue[Range - 1], KeyValue[Range]);
  end;

  { Perform OrientationInterpolator interpolation.
    Range and T are like results of KeyRange call. }
  function InterpolateOrientation(const Interpolator: TOrientationInterpolatorNode;
    const Range: Integer; const T: Single): TVector4;
  var
    KeyValue: TVector4List;
  begin
    KeyValue := Interpolator.FdKeyValue.Items;
    if Range = 0 then
      Result := KeyValue[0]
    else
    if Range = KeyValue.Count then
      Result := KeyValue[KeyValue.Count - 1]
    else
      // SLerp, like TOrientationInterpolatorNode.InterpolatorLerp
      Result := SLerp(T, KeyValue[Range - 1], KeyValue[Range]);
  end;

  { Update all CurrentXxx values affected by this animation. }
  procedure UpdateCurrentTransformation;
  var
    Interpolator: TInterpolator;
    T: Single;
    Range, TargetIndex: Integer;
    Key: TSingleList;
  begin
    for Interpolator in Animation.Interpolators do
    begin
      TargetIndex := TransformNodes.IndexOf(Interpolator.Target);
      if TargetIndex = -1 then
        raise EInternalError.Create('Interpolator.Target not on Nodes list');

      { Below we process time,
        similar to TAbstractSingleInterpolatorNode.EventSet_FractionReceive. }

      Key := Interpolator.Node.FdKey.Items;
      if Key.Count = 0 then
        // Interpolator nodes containing no keys in the key field shall not produce any events.
        Exit;

      Range := KeyRange(Key, Time, T);

      case Interpolator.Path of
        gsTranslation: CurrentTranslation[TargetIndex] :=
          InterpolatePosition(Interpolator.Node as TPositionInterpolatorNode, Range, T);
        gsRotation: CurrentRotation[TargetIndex] :=
          InterpolateOrientation(Interpolator.Node as TOrientationInterpolatorNode, Range, T);
        gsScale: CurrentScale[TargetIndex] :=
          InterpolatePosition(Interpolator.Node as TPositionInterpolatorNode, Range, T);
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('Unexpected glTF Interpolator.Path value');
        {$endif}
      end;
    end;
  end;

  { Calculate contents of Transformations, based on CurrentXxx and parent-child relationships. }
  procedure UpdateMatrix;

    procedure UpdateChildMatrix(const NodeIndex: Integer;
      const ParentT: TTransformation);
    var
      T: PTransformation;
      ChildNodeIndex: Integer;
    begin
      if not Between(NodeIndex, 0, Transformations.Count - 1) then
        Exit; // warning about it was already done by ReadNodes

      T := PTransformation(Transformations.Ptr(NodeIndex));
      T^ := ParentT;
      T^.Multiply(
        CurrentRotation[NodeIndex],
        CurrentScale[NodeIndex],
        CurrentTranslation[NodeIndex]);

      for ChildNodeIndex in TransformNodesGltf[NodeIndex].Children do
        UpdateChildMatrix(ChildNodeIndex, T^);
    end;

  var
    T: PTransformation;
    RootNodeIndex, ChildNodeIndex: Integer;
  begin
    for RootNodeIndex in TransformNodesRoots do
    begin
      if not Between(RootNodeIndex, 0, Transformations.Count - 1) then
        Continue; // warning about it was already done by ReadScene

      T := PTransformation(Transformations.Ptr(RootNodeIndex));
      T^.Init;
      T^.Multiply(
        CurrentRotation[RootNodeIndex],
        CurrentScale[RootNodeIndex],
        CurrentTranslation[RootNodeIndex]);

      for ChildNodeIndex in TransformNodesGltf[RootNodeIndex].Children do
        UpdateChildMatrix(ChildNodeIndex, T^);
    end;
  end;

begin
  { Since in practice TransformNodes.Count is constant during glTF file reading,
    this sets the size only at first SetTime call (for this glTF model). }
  Transformations.Count := TransformNodes.Count;
  CurrentTranslation.Count := TransformNodes.Count;
  CurrentRotation.Count := TransformNodes.Count;
  CurrentScale.Count := TransformNodes.Count;

  ResetCurrentTransformation;
  UpdateCurrentTransformation;
  UpdateMatrix;
end;

{ TTexture ------------------------------------------------------------------- }

type
  { Texture from glTF information.
    Simpler to initialize than TPasGLTF.TMaterial.TTexture.
    ( https://github.com/BeRo1985/pasgltf/blob/master/src/viewer/UnitGLTFOpenGL.pas
    also does it like this, with TTexture record to handle PBRSpecularGlossiness. ) }
  TTexture = record
    Index: TPasGLTFSizeInt;
    TexCoord: TPasGLTFSizeInt;
    TextureTransform: TPasJSONItem;
    procedure Init;
    function Empty: Boolean;
  end;

procedure TTexture.Init;
begin
  Index := -1;
  TexCoord := 0;
  TextureTransform := nil;
end;

function TTexture.Empty: Boolean;
begin
  Result := Index < 0;
end;

{ TPbrMetallicRoughness ------------------------------------------------------ }

type
  TPbrMetallicRoughness = record
    BaseColorFactor: TVector4;
    BaseColorTexture: TTexture;
    MetallicFactor, RoughnessFactor: Single;
    MetallicRoughnessTexture: TTexture;
    { Read glTF material parameters into PBR metallic-roughness model.

      This internally handles PBR specular-glossiness model, converting
      it into metallic-roughness. See
      https://github.com/KhronosGroup/glTF/tree/master/extensions/2.0/Khronos/KHR_materials_pbrSpecularGlossiness
      https://github.com/KhronosGroup/glTF/blob/master/extensions/2.0/Khronos/KHR_materials_pbrSpecularGlossiness/examples/convert-between-workflows/js/three.pbrUtilities.js
    }
    procedure Read(const Material: TPasGLTF.TMaterial);
  end;

procedure TPbrMetallicRoughness.Read(const Material: TPasGLTF.TMaterial);
const
  { Dielectric specular is RGB color with (0.04,0.04,0.04) defined by glTF spec. }
  DielectricSpecular = 0.04;

  { Equations to convert from specular-glossiness to metallic-roughness from
    https://github.com/KhronosGroup/glTF/tree/master/extensions/2.0/Khronos/KHR_materials_pbrSpecularGlossiness/examples/convert-between-workflows/js
  }

  function SolveMetallic(const Diffuse, Specular, OneMinusSpecularStrength: Single): Single;
  var
    A, B, C, D: Single;
  begin
    if Specular < DielectricSpecular then
      Exit(0);

    A := DielectricSpecular;
    B := Diffuse * OneMinusSpecularStrength / (1 - DielectricSpecular) + Specular - 2 * DielectricSpecular;
    C := DielectricSpecular - Specular;
    D := Max(B * B - 4 * A * C, 0);
    Result := Clamped((-B + Sqrt(D)) / (2 * A), 0, 1);
  end;

  function GetPerceivedBrightness(const C: TCastleColorRGB): Single;
  begin
    Result := Sqrt(0.299 * Sqr(C.X) + 0.587 * Sqr(C.Y) + 0.114 * Sqr(C.Z));
  end;

  procedure ConvertSpecularGlossinessToMetallicRoughness(
    const Diffuse: TVector4; const Specular: TVector3;
    const Glossiness: Single; const HasSpecularGlossinessTexture: Boolean);
  var
    Diffuse3: TVector3;
    OneMinusSpecularStrength: Single;
    BaseColorFromDiffuse, BaseColorFromSpecular: TVector3;
  begin
    OneMinusSpecularStrength := 1 - Specular.Max;
    Diffuse3 := Diffuse.XYZ;
    MetallicFactor := SolveMetallic(GetPerceivedBrightness(Diffuse3), GetPerceivedBrightness(Specular),
      OneMinusSpecularStrength);
    BaseColorFromDiffuse := Diffuse3 * (OneMinusSpecularStrength / (1 - DielectricSpecular) / Max(1 - MetallicFactor, SingleEpsilon));
    BaseColorFromSpecular := (Specular -
      (Vector3(DielectricSpecular, DielectricSpecular, DielectricSpecular) * (1 - MetallicFactor))) *
      (1 / Max(MetallicFactor, SingleEpsilon));
    BaseColorFactor := Vector4(
      TVector3.Lerp(Sqr(MetallicFactor), BaseColorFromDiffuse, BaseColorFromSpecular),
      Diffuse.W { BaseColor.alpha is just copied from Diffuse.alpha }
    );
    RoughnessFactor := 1 - Glossiness;

    if HasSpecularGlossinessTexture then
    begin
      { This whole conversion isn't really correct if material has SpecularGlossinessTexture,
        since this changes specular/glossiness per-pixel,
        and we cannot account for that without shader.

        In particular glossiness may be left as 1 (default),
        causing roughness 0, which results in black material,
        for Bee from https://github.com/castle-engine/view3dscene/issues/27 .

        For now just avoid having RoughnessFactor ridiculously low. }
      RoughnessFactor := Max(RoughnessFactor, 0.05);
    end;

    (*
    Writeln('PBR specular-glossiness to metallic-roughness:' + NL +
      '  Input Diffuse ' + Diffuse.ToString + NL +
      '  Input Specular ' + Specular.ToString + NL +
      '  Input Glossiness ', Glossiness:1:2, NL +
      '  ->' + NL +
      '  Output Base ' + BaseColorFactor.ToString + NL +
      '  Output Metallic ', MetallicFactor:1:2, NL +
      '  Output Roughness ', RoughnessFactor:1:2
    ); *)
  end;

  procedure ReadSpecularGlossiness(const JsonSpecGloss: TPasJSONItemObject);
  var
    JSONItem, Exts: TPasJSONItem;
    DiffuseFactor: TVector4;
    SpecularFactor: TVector3;
    GlossinessFactor: Single;
    DiffuseTexture, SpecularGlossinessTexture: TTexture;
    DiffuseJson, SpecularGlossinessJson: TPasJSONItemObject;
  begin
    { Read PBR specular-glossiness subset.
      As we only read subset, we use it only when
      Material.PBRMetallicRoughness is empty, despite
      https://github.com/KhronosGroup/glTF/tree/main/extensions/2.0/Archived/KHR_materials_pbrSpecularGlossiness
      advising to use specular-glossiness if you can.

      Code below, to read specular-glossiness from JSON, is based on PasGLTF UnitGLTFOpenGL. }

    DiffuseFactor := Vector4(1, 1, 1, 1); // default
    JSONItem := JsonSpecGloss.Properties['diffuseFactor'];
    if Assigned(JSONItem) and
       (JSONItem is TPasJSONItemArray) and
       (TPasJSONItemArray(JSONItem).Count = 4) then
    begin
      DiffuseFactor.X := TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0], 1);
      DiffuseFactor.Y := TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[1], 1);
      DiffuseFactor.Z := TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[2], 1);
      DiffuseFactor.W := TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[3], 1);
    end;

    DiffuseTexture.Init;
    JSONItem := JsonSpecGloss.Properties['diffuseTexture'];
    if Assigned(JSONItem) and
       (JSONItem is TPasJSONItemObject) then
    begin
      DiffuseJson := TPasJSONItemObject(JSONItem);
      DiffuseTexture.Index := TPasJSON.GetInt64(DiffuseJson.Properties['index'], -1);
      DiffuseTexture.TexCoord := TPasJSON.GetInt64(DiffuseJson.Properties['texCoord'], 0);
      Exts := DiffuseJson.Properties['extensions'];
      if Exts is TPasJSONItemObject then // also checks Exts <> nil
        DiffuseTexture.TextureTransform := TPasJSONItemObject(Exts).Properties['KHR_texture_transform'];
    end;

    GlossinessFactor := TPasJSON.GetNumber(JsonSpecGloss.Properties['glossinessFactor'], 1);

    SpecularFactor := Vector3(1, 1, 1); // default
    JSONItem := JsonSpecGloss.Properties['specularFactor'];
    if Assigned(JSONItem) and
       (JSONItem is TPasJSONItemArray) and
       (TPasJSONItemArray(JSONItem).Count = 3) then
    begin
      SpecularFactor.X := TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[0], 1);
      SpecularFactor.Y := TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[1], 1);
      SpecularFactor.Z := TPasJSON.GetNumber(TPasJSONItemArray(JSONItem).Items[2], 1);
    end;

    SpecularGlossinessTexture.Init;
    JSONItem := JsonSpecGloss.Properties['specularGlossinessTexture'];
    if Assigned(JSONItem) and
      (JSONItem is TPasJSONItemObject) then
    begin
      SpecularGlossinessJson := TPasJSONItemObject(JSONItem);
      SpecularGlossinessTexture.Index := TPasJSON.GetInt64(SpecularGlossinessJson.Properties['index'], -1);
      SpecularGlossinessTexture.TexCoord := TPasJSON.GetInt64(SpecularGlossinessJson.Properties['texCoord'], 0);
      Exts := SpecularGlossinessJson.Properties['extensions'];
      if Exts is TPasJSONItemObject then // also checks Exts <> nil
        SpecularGlossinessTexture.TextureTransform := TPasJSONItemObject(Exts).Properties['KHR_texture_transform'];
    end;

    // convert to metallic-roughness
    ConvertSpecularGlossinessToMetallicRoughness(
      DiffuseFactor, SpecularFactor, GlossinessFactor,
      not SpecularGlossinessTexture.Empty);
    BaseColorTexture := DiffuseTexture;
    // ignore SpecularSpecularGlossinessTexture
  end;

var
  JsonSpecGlossItem: TPasJSONItem;
begin
  BaseColorFactor := Vector4FromGltf(Material.PBRMetallicRoughness.BaseColorFactor);

  BaseColorTexture.Init;
  BaseColorTexture.Index := Material.PBRMetallicRoughness.BaseColorTexture.Index;
  BaseColorTexture.TexCoord := Material.PBRMetallicRoughness.BaseColorTexture.TexCoord;
  BaseColorTexture.TextureTransform := Material.PBRMetallicRoughness.BaseColorTexture.Extensions.Properties['KHR_texture_transform'];

  MetallicFactor := Material.PBRMetallicRoughness.MetallicFactor;
  RoughnessFactor := Material.PBRMetallicRoughness.RoughnessFactor;

  MetallicRoughnessTexture.Init;
  MetallicRoughnessTexture.Index := Material.PBRMetallicRoughness.MetallicRoughnessTexture.Index;
  MetallicRoughnessTexture.TexCoord := Material.PBRMetallicRoughness.MetallicRoughnessTexture.TexCoord;
  MetallicRoughnessTexture.TextureTransform := Material.PBRMetallicRoughness.MetallicRoughnessTexture.Extensions.Properties['KHR_texture_transform'];

  { Read PBR specular-glossiness }

  JsonSpecGlossItem := Material.Extensions.Properties['KHR_materials_pbrSpecularGlossiness'];
  if Material.PBRMetallicRoughness.Empty and
     (JsonSpecGlossItem is TPasJSONItemObject) then
  begin
    WritelnWarning('Material "%s" has only PBR specular-glossiness parameters. We support it only partially (in particular specularGlossinessTexture is ignored). Better use metallic-roughness model.', [
      Material.Name
    ]);
    ReadSpecularGlossiness(TPasJSONItemObject(JsonSpecGlossItem));
  end;
end;

{ TPunctualLights ------------------------------------------------------------------ }

type
  { Load glTF punctual lights.
    Following glTF https://github.com/KhronosGroup/glTF/tree/master/extensions/2.0/Khronos/KHR_lights_punctual
    extension.
    Code was reworked from PasGLTF LoadLights in
    https://github.com/BeRo1985/pasgltf/blob/master/src/viewer/UnitGLTFOpenGL.pas }
  TPunctualLights = class
  strict private
    { Only instances of TAbstractLightNode here. }
    Lights: TX3DNodeList;
    procedure ReadLight(const LightObject: TPasJSONItemObject);
  public
    destructor Destroy; override;
    { Read document header. }
    procedure ReadHeader(const Document: TPasGLTF.TDocument);
    { Read glTF node, and optionally add stuff to given TTransformNode for this node. }
    procedure ReadNode(const Node: TPasGLTF.TNode; const Transform: TTransformNode);
  end;

destructor TPunctualLights.Destroy;
var
  I: Integer;
begin
  if Lights <> nil then
  begin
    for I := 0 to Lights.Count - 1 do
      Lights[I].FreeIfUnused;
    FreeAndNil(Lights);
  end;
  inherited;
end;

procedure TPunctualLights.ReadHeader(const Document: TPasGLTF.TDocument);
var
  KHRLightsPunctualItem, LightsItem, LightItem: TPasJSONItem;
  KHRLightsPunctualObject: TPasJSONItemObject;
  LightsArray: TPasJSONItemArray;
  LightIndex: Integer;
begin
  if Assigned(Document.Extensions) then
  begin
    KHRLightsPunctualItem := Document.Extensions.Properties['KHR_lights_punctual'];
    if Assigned(KHRLightsPunctualItem) and
       (KHRLightsPunctualItem is TPasJSONItemObject) then
    begin
      KHRLightsPunctualObject := TPasJSONItemObject(KHRLightsPunctualItem);
      LightsItem := KHRLightsPunctualObject.Properties['lights'];
      if Assigned(LightsItem) and
         (LightsItem is TPasJSONItemArray) then
      begin
        Lights := TX3DNodeList.Create(false);
        LightsArray := TPasJSONItemArray(LightsItem);
        for LightIndex := 0 to LightsArray.Count - 1 do
        begin
          LightItem := LightsArray.Items[LightIndex];
          if Assigned(LightItem) and
             (LightItem is TPasJSONItemObject) then
          begin
            ReadLight(TPasJSONItemObject(LightItem));
          end;
        end;
      end;
    end;
  end;
end;

procedure TPunctualLights.ReadLight(const LightObject: TPasJSONItemObject);

  { Read color from JSON, specified like light's color spec on
    https://github.com/KhronosGroup/glTF/tree/master/extensions/2.0/Khronos/KHR_lights_punctual
    dictates. }
  function ReadColor(const Item: TPasJSONItem; const DefaultColor: TCastleColorRGB): TCastleColorRGB;
  var
    ColorArray: TPasJSONItemArray;
    I: Integer;
  begin
    Result := DefaultColor;
    if Assigned(Item) and
       (Item is TPasJSONItemArray) then
    begin
      ColorArray := TPasJSONItemArray(Item);
      for I := 0 to Min(2, ColorArray.Count - 1) do
        Result.InternalData[I] := TPasJSON.GetNumber(ColorArray.Items[I], DefaultColor[I]);
    end;
  end;

const
  DefaultBeamWidth = 0.0;
  DefaultCutOffAngle = Pi * 0.25;
var
  SpotItem: TPasJSONItem;
  SpotObject: TPasJSONItemObject;
  TypeString: String;
  Light: TAbstractLightNode;
begin
  TypeString := TPasJSON.GetString(LightObject.Properties['type'], '');

  if TypeString = 'directional' then
  begin
    Light := TDirectionalLightNode.Create;
    TDirectionalLightNode(Light).Direction := Vector3(0, 0, -1);
  end else
  if TypeString = 'point' then
    Light := TPointLightNode.Create
  else
  if TypeString = 'spot' then
  begin
    Light := TSpotLightNode.Create;
    TSpotLightNode(Light).Direction := Vector3(0, 0, -1);

    SpotItem := LightObject.Properties['spot'];
    if Assigned(SpotItem) and
       (SpotItem is TPasJSONItemObject) then
    begin
      SpotObject := TPasJSONItemObject(SpotItem);
      TSpotLightNode(Light).BeamWidth := TPasJSON.GetNumber(SpotObject.Properties['innerConeAngle'], DefaultBeamWidth);
      TSpotLightNode(Light).CutOffAngle := TPasJSON.GetNumber(SpotObject.Properties['outerConeAngle'], DefaultCutOffAngle);
    end else
    begin
      TSpotLightNode(Light).BeamWidth := DefaultBeamWidth;
      TSpotLightNode(Light).CutOffAngle := DefaultCutOffAngle;
    end;
  end else
  begin
    WritelnWarning('Invalid glTF light type "%s"', [TypeString]);
    Exit;
  end;

  Light.Global := true;
  Light.X3DName := TPasJSON.GetString(LightObject.Properties['name'], '');
  { Note that glTF intensity may be very large according to KHR_lights_punctual
    (there is no upper limit, since in physics it has no upper limit).
    And Blender can indeed set it to a very large value, like 1000,
    see castle-engine/examples/fps_game/data/example_level/ .
    This is not a problem for CGE,
    and X3D >= 4.0 also allows any large intensity.
    (only X3D 3 and VRML limited it to [0..1] range). }
  Light.Intensity := TPasJSON.GetNumber(LightObject.Properties['intensity'], 1);
  Light.Color := ReadColor(LightObject.Properties['color'], WhiteRGB);

  if Light is TAbstractPositionalLightNode then
  begin
    // TODO: falloff to this range will not be as specified by glTF
    TAbstractPositionalLightNode(Light).Radius := TPasJSON.GetNumber(LightObject.Properties['range'], 0);
    // glTF expresses "no range" as 0, we express it as -1 in X3D
    if TAbstractPositionalLightNode(Light).Radius = 0 then
      TAbstractPositionalLightNode(Light).Radius := -1;

    { Following KHR_lights_punctual: """When undefined, range is assumed
      to be infinite and the light should attenuate according to inverse square law."""
      Simply using this attenuation makes us correct, e.g. rendering matches
      https://gltf-viewer.donmccurdy.com/ (testcase: fps_game level,
      demo-models/gltf/punctual_lights/ ).

      Note that without this, X3D by default makes no attenuation,
      which would be bad with large intensity of lights (the scene would be incredibly bright).

      TODO: When range is defined, we should make a different falloff according to
      KHR_lights_punctual. }
    TAbstractPositionalLightNode(Light).Attenuation := Vector3(0, 0, 1);
  end;

  Lights.Add(Light);
end;

procedure TPunctualLights.ReadNode(const Node: TPasGLTF.TNode; const Transform: TTransformNode);
var
  LightIndex: Int64;
  KHRLightsPunctualItem: TPasJSONItem;
  KHRLightsPunctualObject: TPasJSONItemObject;
begin
  if (Lights <> nil) and Assigned(Node.Extensions) then
  begin
    KHRLightsPunctualItem := Node.Extensions.Properties['KHR_lights_punctual'];
    if Assigned(KHRLightsPunctualItem) and
       (KHRLightsPunctualItem is TPasJSONItemObject) then
    begin
      KHRLightsPunctualObject := TPasJSONItemObject(KHRLightsPunctualItem);
      LightIndex := TPasJSON.GetInt64(KHRLightsPunctualObject.Properties['light'], -1);
      if Between(LightIndex, 0, Lights.Count - 1) then
        Transform.AddChildren(Lights[LightIndex] as TAbstractLightNode)
      else
        WritelnWarning('Invalid light index %d', [LightIndex]);
    end;
  end;
end;

{ TTextureTransforms --------------------------------------------------------- }

type
  { To connect glTF idea of texture transformations (each textureInfo can
    have texture transformation) with X3D 4 (each mapping can have texture transformation),
    this collects current texture transformation requirements.

    TODO: We could support more even without extending X3D:
    different tex coords could have different transformations. }
  TTextureTransforms = class
  strict private
    HasAny: Boolean;
  public
    SingleTextureTransform: TTextureTransformNode;

    { Mark that given TexCoords have to be transformed with given TTextureTransformNode
      (which may be @nil).

      Ignored when TexCoords = '' (this means we don't have a texture, ReadTexture returns
      mapping = '' then).

      When TextureTransform = nil means that these tex coords should not be transformed.
      (This is still valuable information that you should pass for each texture.) }
    procedure TransformCoords(const TexCoord: String; const TextureTransform: TTextureTransformNode);
  end;

procedure TTextureTransforms.TransformCoords(const TexCoord: String; const TextureTransform: TTextureTransformNode);
begin
  if HasAny then
  begin
    if SingleTextureTransform <> TextureTransform then
    begin
      if (SingleTextureTransform <> nil) and
         (TextureTransform <> nil) and
         TVector2.PerfectlyEquals(SingleTextureTransform.FdTranslation.Value, TextureTransform.FdTranslation.Value) and
         TVector2.PerfectlyEquals(SingleTextureTransform.FdScale.Value, TextureTransform.FdScale.Value) and
         (SingleTextureTransform.FdRotation.Value = TextureTransform.FdRotation.Value) then
        { Ignore the difference, contents of TTextureTransformNode are equal. }
        Exit;
      WritelnWarning('TODO: Textures within material have different texture transformation, not supported now');
      FreeIfUnusedAndNil(SingleTextureTransform);
    end;
  end;

  HasAny := true;
  SingleTextureTransform := TextureTransform;
end;

{ LoadGltf ------------------------------------------------------------------- }

{ Main routine that converts glTF -> X3D nodes, doing most of the work. }
function LoadGltf(const Stream: TStream; const BaseUrl: String): TX3DRootNode;
var
  Document: TPasGLTF.TDocument;
  // List of TGltfAppearanceNode nodes, ordered just list glTF materials
  Appearances: TX3DNodeList;
  { List of TTransformNode nodes, ordered just list glTF nodes.
    Only initialized (non-nil and enough Count) for nodes that we created in ReadNode. }
  Nodes: TX3DNodeList;
  { List of X3D nodes to be EXPORTed from the glTF scene,
    so that outer X3D can IMPORT them and use.
    Nodes with X3DName = '' on this list are ignored.
    Everything on Appearances, Nodes, Animations is already EXPORTed too. }
  ExportNodes: TX3DNodeList;
  DefaultAppearance: TGltfAppearanceNode;
  SkinsToInitialize: TSkinToInitializeList;
  Animations: TAnimationList;
  AnimationSampler: TAnimationSampler;
  JointMatrix: TMatrix4List; //< local for SampleSkinAnimation, but created once to avoid wasting time on allocation
  Lights: TPunctualLights;

  procedure ReadHeader;
  const
    SupportedExtensions: array [0..3] of String = (
      'KHR_materials_pbrSpecularGlossiness',
      'KHR_texture_transform',
      'KHR_lights_punctual',
      'KHR_materials_unlit'
    );
  var
    ExtRequired: String;
  begin
    // too verbose to be done by default
    (*
    WritelnLogMultiline('glTF', Format(
      'Asset.Copyright: %s' + NL +
      'Asset.Generator: %s' + NL +
      'Asset.MinVersion: %s' + NL +
      'Asset.Version: %s' + NL +
      'Asset.Empty: %s' + NL +
      'Accessors: %d' + NL +
      'Animations: %d' + NL +
      'Buffers: %d' + NL +
      'BufferViews: %d' + NL +
      'Cameras: %d' + NL +
      'Images: %d' + NL +
      'Materials: %d' + NL +
      'Meshes: %d' + NL +
      'Nodes: %d' + NL +
      'Samplers: %d' + NL +
      'Scenes: %d' + NL +
      'Skins: %d' + NL +
      'Textures: %d' + NL +
      'ExtensionsUsed: %s' + NL +
      'ExtensionsRequired: %s' + NL +
      '', [
        Document.Asset.Copyright,
        Document.Asset.Generator,
        Document.Asset.MinVersion,
        Document.Asset.Version,
        BoolToStr(Document.Asset.Empty, true),

        Document.Accessors.Count,
        Document.Animations.Count,
        Document.Buffers.Count,
        Document.BufferViews.Count,
        Document.Cameras.Count,
        Document.Images.Count,
        Document.Materials.Count,
        Document.Meshes.Count,
        Document.Nodes.Count,
        Document.Samplers.Count,
        Document.Scenes.Count,
        Document.Skins.Count,
        Document.Textures.Count,
        Document.ExtensionsUsed.Text,
        Document.ExtensionsRequired.Text
      ])
    );
    *)
    for ExtRequired in Document.ExtensionsRequired do
      if ArrayPosStr(ExtRequired, SupportedExtensions) = -1 then
        WritelnWarning('Required extension "%s" not supported by glTF reader', [ExtRequired]);
  end;

  { Read glTF "extras" item, with given key and value (JSON array), into X3D "metadata" information. }
  procedure ReadMetadataKeyValueFromArray(const Key: String; const JsonArray: TPasJSONItemArray; const Node: TAbstractNode);
  var
    J: Integer;
  begin
    if JsonArray.Count > 0 then // we ignore empty arrays, as their metadata type is unknown
    begin
      if JsonArray[0] is TPasJSONItemString then
      begin
        Node.MetadataStringArray[Key, JsonArray.Count - 1] := ''; // set array size
        for J := 0 to JsonArray.Count - 1 do
        begin
          if not (JsonArray[J] is TPasJSONItemString) then
          begin
            WritelnWarning('Cannot read glTF extra "%s" index %d, different type than 1st array item', [
              Key,
              J
            ]);
            Continue;
          end;
          Node.MetadataStringArray[Key, J] := (JsonArray[J] as TPasJSONItemString).Value;
        end;
      end else
      if JsonArray[0] is TPasJSONItemBoolean then
      begin
        Node.MetadataBooleanArray[Key, JsonArray.Count - 1] := false; // set array size
        for J := 0 to JsonArray.Count - 1 do
        begin
          if not (JsonArray[J] is TPasJSONItemBoolean) then
          begin
            WritelnWarning('Cannot read glTF extra "%s" index %d, different type than 1st array item', [
              Key,
              J
            ]);
            Continue;
          end;
          Node.MetadataBooleanArray[Key, J] := (JsonArray[J] as TPasJSONItemBoolean).Value;
        end;
      end else
      if JsonArray[0] is TPasJSONItemNumber then
      begin
        Node.MetadataDoubleArray[Key, JsonArray.Count - 1] := 0; ; // set array size
        for J := 0 to JsonArray.Count - 1 do
        begin
          if not (JsonArray[J] is TPasJSONItemNumber) then
          begin
            WritelnWarning('Cannot read glTF extra "%s" index %d, different type than 1st array item', [
              Key,
              J
            ]);
            Continue;
          end;
          Node.MetadataDoubleArray[Key, J] := (JsonArray[J] as TPasJSONItemNumber).Value;
        end;
      end else
      begin
        WritelnWarning('Cannot read glTF extra "%s", unexpected type inside array %s', [
          Key,
          JsonArray[0].ClassName
        ]);
      end;
    end;
  end;

  { Read glTF "extras" item, with given key and value (JSON object), into X3D "metadata" information.
    Testcase: https://github.com/KhronosGroup/3DC-Certification/tree/main/models/AnalyticalCubes }
  procedure ReadMetadataKeyValueFromObject(const Key: String; const JsonObject: TPasJSONItemObject; const Node: TAbstractNode);
  var
    MetadataSet: TMetadataSetNode;
    StrNode: TMetadataStringNode;
    BoolNode: TMetadataBooleanNode;
    DoubleNode: TMetadataDoubleNode;
    I: Integer;
  begin
    { TODO: It would be better to use recursion here to do read metadata. }
    MetadataSet := TMetadataSetNode.Create;
    MetadataSet.NameField := Key;
    Node.InternalInsertMetadata(MetadataSet);

    for I := 0 to JsonObject.Count - 1 do
    begin
      if JsonObject.Values[I] is TPasJSONItemString then
      begin
        StrNode := TMetadataStringNode.Create;
        StrNode.NameField := JsonObject.Keys[I];
        StrNode.SetValue([TPasJSONItemString(JsonObject.Values[I]).Value]);
        MetadataSet.FdValue.Add(StrNode);
      end else
      if JsonObject.Values[I] is TPasJSONItemBoolean then
      begin
        BoolNode := TMetadataBooleanNode.Create;
        BoolNode.NameField := JsonObject.Keys[I];
        BoolNode.SetValue([TPasJSONItemBoolean(JsonObject.Values[I]).Value]);
        MetadataSet.FdValue.Add(BoolNode);
      end else
      if JsonObject.Values[I] is TPasJSONItemNumber then
      begin
        DoubleNode := TMetadataDoubleNode.Create;
        DoubleNode.NameField := JsonObject.Keys[I];
        DoubleNode.SetValue([TPasJSONItemNumber(JsonObject.Values[I]).Value]);
        MetadataSet.FdValue.Add(DoubleNode);
      end else
      begin
        WritelnWarning('Cannot read glTF object "%s" inside extra metadata, field "%s", unhandled type %s', [
          Key,
          JsonObject.Keys[I],
          JsonObject.Values[I].ClassName
        ]);
      end;
    end;
  end;

  { Read glTF "extras" into X3D "metadata" information. }
  procedure ReadMetadata(const Extras: TPasJSONItemObject; const Node: TAbstractNode);
  var
    I: Integer;
    Key: String;
  begin
    for I := 0 to Extras.Count - 1 do
    begin
      Key := Extras.Keys[I];
      if Extras.Values[I] is TPasJSONItemString then
        Node.MetadataString[Key] := TPasJSONItemString(Extras.Values[I]).Value
      else
      if Extras.Values[I] is TPasJSONItemBoolean then
        Node.MetadataBoolean[Key] := TPasJSONItemBoolean(Extras.Values[I]).Value
      else
      if Extras.Values[I] is TPasJSONItemNumber then
        Node.MetadataDouble[Key] := TPasJSONItemNumber(Extras.Values[I]).Value
      else
      if Extras.Values[I] is TPasJSONItemArray then
        ReadMetadataKeyValueFromArray(Key, TPasJSONItemArray(Extras.Values[I]), Node)
      else
      if Extras.Values[I] is TPasJSONItemObject then
        ReadMetadataKeyValueFromObject(Key, TPasJSONItemObject(Extras.Values[I]), Node)
      else
      begin
        WritelnWarning('Cannot read glTF extra "%s", unexpected type %s', [
          Key,
          Extras.Values[I].ClassName
        ]);
      end;
    end;
  end;

  function ReadTextureWrap(const Wrap: TPasGLTF.TSampler.TWrappingMode): TBoundaryMode;
  begin
    case Wrap of
      TPasGLTF.TSampler.TWrappingMode.Repeat_       : Result := bmRepeat;
      TPasGLTF.TSampler.TWrappingMode.ClampToEdge   : Result := bmClampToEdge;
      TPasGLTF.TSampler.TWrappingMode.MirroredRepeat: Result := bmMirroredRepeat;
      else raise EInternalError.Create('Unexpected glTF wrap');
    end;
  end;

  function ReadMinificationFilter(const Filter: TPasGLTF.TSampler.TMinFilter): TAutoMinificationFilter;
  begin
    case Filter of
      TPasGLTF.TSampler.TMinFilter.None                : Result := minDefault;
      TPasGLTF.TSampler.TMinFilter.Nearest             : Result := minNearest;
      TPasGLTF.TSampler.TMinFilter.Linear              : Result := minLinear;
      TPasGLTF.TSampler.TMinFilter.NearestMipMapNearest: Result := minNearestMipmapNearest;
      TPasGLTF.TSampler.TMinFilter.LinearMipMapNearest : Result := minLinearMipmapNearest;
      TPasGLTF.TSampler.TMinFilter.NearestMipMapLinear : Result := minNearestMipmapLinear;
      TPasGLTF.TSampler.TMinFilter.LinearMipMapLinear  : Result := minLinearMipmapLinear;
      else raise EInternalError.Create('Unexpected glTF minification filter');
    end;
  end;

  function ReadMagnificationFilter(const Filter: TPasGLTF.TSampler.TMagFilter): TAutoMagnificationFilter;
  begin
    case Filter of
      TPasGLTF.TSampler.TMagFilter.None   : Result := magDefault;
      TPasGLTF.TSampler.TMagFilter.Nearest: Result := magNearest;
      TPasGLTF.TSampler.TMagFilter.Linear : Result := magLinear;
      else raise EInternalError.Create('Unexpected glTF magnification filter');
    end;
  end;

  function FixTextureUrl(const Url: String): String;
  begin
    Result := Url;

    { Workaround https://github.com/castle-engine/castle-engine/issues/339 }
    if Pos('\', Result) <> 0 then
    begin
      WritelnWarning('URL in glTF contains a backslash "\", assuming that slash was meant "/": "%s"', [
        Result
      ]);
      StringReplaceAllVar(Result, '\', '/');
    end;
  end;

  { Read glTF texture transform from KHR_texture_transform JSON item following
    https://github.com/KhronosGroup/glTF/tree/main/extensions/2.0/Khronos/KHR_texture_transform .
    Input GltfTextureTransform may be @nil or not even TPasJSONItemObject.
    May return @nil if no texture transformation.

    Note: glTF texture V coordinates go down, not up, as in CGE and X3D.
    But we don't need to do anything about it here, because we are flipping the textures,
    so there's no need to e.g. flip offset.y in KHR_texture_transform .
  }
  function ReadTextureTransform(const GltfTextureTransform: TPasJSONItem): TTextureTransformNode;
  var
    TextureTransformJson: TPasJSONItemObject;
    Offset, Scale: TPasJSONItem;
  begin
    Result := nil;
    if GltfTextureTransform is TPasJSONItemObject then
    begin
      TextureTransformJson := TPasJSONItemObject(GltfTextureTransform);
      Result := TTextureTransformNode.Create;

      Offset := TextureTransformJson.Properties['offset'];
      if (Offset is TPasJSONItemArray) and (TPasJSONItemArray(Offset).Count = 2) then
        Result.FdTranslation.Value := Vector2(
          TPasJSON.GetNumber(TPasJSONItemArray(Offset).Items[0], 0.0),
          TPasJSON.GetNumber(TPasJSONItemArray(Offset).Items[1], 0.0)
        );

      Result.FdRotation.Value := -TPasJSON.GetNumber(TextureTransformJson.Properties['rotation'], 0);

      Scale := TextureTransformJson.Properties['scale'];
      if (Scale is TPasJSONItemArray) and (TPasJSONItemArray(Scale).Count = 2) then
        Result.FdScale.Value := Vector2(
          TPasJSON.GetNumber(TPasJSONItemArray(Scale).Items[0], 1.0),
          TPasJSON.GetNumber(TPasJSONItemArray(Scale).Items[1], 1.0)
        );
    end;
  end;

  procedure ReadTexture(const GltfTextureAtMaterial: TTexture;
    out Texture: TAbstractTexture2DNode; out TexMapping: String;
    const TextureTransforms: TTextureTransforms); overload;
  var
    GltfTexture: TPasGLTF.TTexture;
    GltfImage: TPasGLTF.TImage;
    GltfSampler: TPasGLTF.TSampler;
    TextureProperties: TTexturePropertiesNode;
    Stream: TMemoryStream;
    TexCoord, OverrideTexCoord: TPasGLTFSizeInt;
    TextureTransform: TTextureTransformNode;
  begin
    Texture := nil;
    TextureTransform := nil;
    GltfImage := nil;
    TexMapping := ''; // for no texture, use empty mapping, to keep output X3D simple

    if not GltfTextureAtMaterial.Empty then
    begin
      if GltfTextureAtMaterial.Index < Document.Textures.Count then
      begin
        GltfTexture := Document.Textures[GltfTextureAtMaterial.Index];

        if Between(GltfTexture.Source, 0, Document.Images.Count - 1) then
        begin
          GltfImage := Document.Images[GltfTexture.Source];
          if GltfImage.URI <> '' then
          begin
            if FfmpegVideoMimeType(URIMimeType(GltfImage.URI), false) then
            begin
              Texture := TMovieTextureNode.Create('', BaseUrl);
              TMovieTextureNode(Texture).SetUrl([FixTextureUrl(GltfImage.URI)]);
              if CastleX3dExtensions then
                TMovieTextureNode(Texture).FlipVertically := true;
              TMovieTextureNode(Texture).Loop := true;
            end else
            begin
              Texture := TImageTextureNode.Create('', BaseUrl);
              TImageTextureNode(Texture).SetUrl([FixTextureUrl(GltfImage.URI)]);

              { glTF specification defines (0,0) texture coord to be
                at top-left corner, while X3D and OpenGL and OpenGLES expect it be
                at bottom-left corner.
                See
                https://castle-engine.io/x3d_implementation_texturing_extensions.php#section_flip_vertically
                for a detailed discussion.

                So we flip the textures.
                This way we can use original texture coordinates from glTF
                file (no need to process them, by doing "y := 1 - y"). }
              if CastleX3dExtensions then
                TImageTextureNode(Texture).FlipVertically := true;
            end;
          end else
          if GltfImage.BufferView >= 0 then
          begin
            { Use GltfImage.GetResourceData to load from buffer
              (instead of an external file). In particular, this is necessary to
              support GLB format with textures.

              Note that we use GltfImage.GetResourceData only when
              GltfImage.BufferView was set. Otherwise, we want to interpret URI
              by CGE code, thus allowing to read files using our Download()
              that understands also http/https, castle-data, castle-android-assets etc.
            }
            Stream := TMemoryStream.Create;
            try
              GltfImage.GetResourceData(Stream);
              Stream.Position := 0;

              { TODO: In case this is a DDS/KTX file, by using LoadImage
                we lose information about additional mipmaps,
                cubemap faces etc. }

              Texture := TPixelTextureNode.Create;
              try
                TPixelTextureNode(Texture).FdImage.Value :=
                  LoadImage(Stream, GltfImage.MimeType, []);
              except
                on E: Exception do
                  WritelnWarning('glTF', 'Cannot load the texture from glTF binary buffer with mime type %s: %s',
                    [GltfImage.MimeType, ExceptMessage(E)]);
              end;

              { Same reason as for TImageTextureNode.FlipVertically above:
                glTF specification defines (0,0) texture coord to be
                at top-left corner. }
              if CastleX3dExtensions then
                TPixelTextureNode(Texture).FdImage.Value.FlipVertical;
            finally FreeAndNil(Stream) end;
          end;
        end;

        if Texture <> nil then // above clause succeded in reading Texture
        begin
          { Use glTF name for X3D node name. }
          if GltfImage <> nil then
            Texture.X3DName := GltfImage.Name;

          TexCoord := GltfTextureAtMaterial.TexCoord;

          { Allow KHR_texture_transform to override texture coordinates,
            if it specifies texCoord. }
          if GltfTextureAtMaterial.TextureTransform is TPasJSONItemObject then
          begin
            OverrideTexCoord := TPasJSON.GetInt64(TPasJSONItemObject(GltfTextureAtMaterial.TextureTransform).Properties['texCoord'], -1);
            if OverrideTexCoord <> -1 then
              TexCoord := OverrideTexCoord;
          end;

          TexMapping := 'TEXCOORD_' + IntToStr(TexCoord);

          // read wrap and filtering options
          if Between(GltfTexture.Sampler, 0, Document.Samplers.Count - 1) then
          begin
            GltfSampler := Document.Samplers[GltfTexture.Sampler];

            if (GltfSampler.WrapS <> TPasGLTF.TSampler.TWrappingMode.Repeat_) or
               (GltfSampler.WrapT <> TPasGLTF.TSampler.TWrappingMode.Repeat_) or
               (GltfSampler.MinFilter <> TPasGLTF.TSampler.TMinFilter.None) or
               (GltfSampler.MagFilter <> TPasGLTF.TSampler.TMagFilter.None) then
            begin
              TextureProperties := TTexturePropertiesNode.Create;
              TextureProperties.MinificationFilter := ReadMinificationFilter(GltfSampler.MinFilter);
              TextureProperties.MagnificationFilter := ReadMagnificationFilter(GltfSampler.MagFilter);
              TextureProperties.BoundaryModeS := ReadTextureWrap(GltfSampler.WrapS);
              TextureProperties.BoundaryModeT := ReadTextureWrap(GltfSampler.WrapT);
              Texture.TextureProperties := TextureProperties;
            end;
          end;

          TextureTransform := ReadTextureTransform(GltfTextureAtMaterial.TextureTransform);
          TextureTransforms.TransformCoords(TexMapping, TextureTransform);
        end;
      end;
    end;
  end;

  procedure ReadTexture(const GltfTextureAtMaterial: TPasGLTF.TMaterial.TTexture;
    out Texture: TAbstractTexture2DNode; out TexMapping: String;
    const TextureTransforms: TTextureTransforms); overload;
  var
    TextureRec: TTexture;
  begin
    TextureRec.Init;
    TextureRec.Index := GltfTextureAtMaterial.Index;
    TextureRec.TexCoord := GltfTextureAtMaterial.TexCoord;
    TextureRec.TextureTransform := GltfTextureAtMaterial.Extensions.Properties['KHR_texture_transform'];
    ReadTexture(TextureRec, Texture, TexMapping, TextureTransforms);
  end;

  function ReadPhongMaterial(const Material: TPasGLTF.TMaterial;
    const TexTransforms: TTextureTransforms): TMaterialNode;
  var
    PbrMetallicRoughness: TPbrMetallicRoughness;
    BaseColorTexture, NormalTexture, EmissiveTexture: TAbstractTexture2DNode;
    BaseColorTextureMapping, NormalTextureMapping, EmissiveTextureMapping: String;
    // MetallicFactor, RoughnessFactor: Single;
  begin
    PbrMetallicRoughness.Read(Material);

    Result := TMaterialNode.Create;
    Result.DiffuseColor := PbrMetallicRoughness.BaseColorFactor.XYZ;
    Result.Transparency := 1 - PbrMetallicRoughness.BaseColorFactor.W;
    Result.EmissiveColor := Vector3FromGltf(Material.EmissiveFactor);

    // Metallic/roughness conversion idea from X3DOM.
    // Gives weird artifacts on some samples (Duck, FlightHelmet) so not used now.
    (*
    MetallicFactor := PbrMetallicRoughness.MetallicFactor;
    RoughnessFactor := PbrMetallicRoughness.RoughnessFactor;
    Result.SpecularColor := Vector3(
      Lerp(MetallicFactor, 0.04, BaseColorFactor.X),
      Lerp(MetallicFactor, 0.04, BaseColorFactor.Y),
      Lerp(MetallicFactor, 0.04, BaseColorFactor.Z)
    );
    Result.Shininess := 1 - RoughnessFactor;
    *)

    ReadTexture(PbrMetallicRoughness.BaseColorTexture, BaseColorTexture, BaseColorTextureMapping, TexTransforms);
    Result.DiffuseTexture := BaseColorTexture;
    Result.DiffuseTextureMapping := BaseColorTextureMapping;

    ReadTexture(Material.NormalTexture,
      NormalTexture, NormalTextureMapping, TexTransforms);
    Result.NormalTexture := NormalTexture;
    Result.NormalTextureMapping := NormalTextureMapping;

    if not Material.NormalTexture.Empty then
      Result.NormalScale := Material.NormalTexture.Scale;

    ReadTexture(Material.EmissiveTexture,
      EmissiveTexture, EmissiveTextureMapping, TexTransforms);
    Result.EmissiveTexture := EmissiveTexture;
    Result.EmissiveTextureMapping := EmissiveTextureMapping;
  end;

  function ReadPhysicalMaterial(const Material: TPasGLTF.TMaterial;
    const TexTransforms: TTextureTransforms): TPhysicalMaterialNode;
  var
    PbrMetallicRoughness: TPbrMetallicRoughness;
    BaseColorTexture, NormalTexture, EmissiveTexture, MetallicRoughnessTexture, OcclusionTexture: TAbstractTexture2DNode;
    BaseColorTextureMapping, NormalTextureMapping, EmissiveTextureMapping, MetallicRoughnessTextureMapping, OcclusionTextureMapping: String;
  begin
    PbrMetallicRoughness.Read(Material);

    Result := TPhysicalMaterialNode.Create;
    Result.BaseColor := PbrMetallicRoughness.BaseColorFactor.XYZ;
    Result.Transparency := 1 - PbrMetallicRoughness.BaseColorFactor.W;
    Result.Metallic := PbrMetallicRoughness.MetallicFactor;
    Result.Roughness := PbrMetallicRoughness.RoughnessFactor;
    Result.EmissiveColor := Vector3FromGltf(Material.EmissiveFactor);

    ReadTexture(PbrMetallicRoughness.BaseColorTexture,
      BaseColorTexture, BaseColorTextureMapping, TexTransforms);
    Result.BaseTexture := BaseColorTexture;
    Result.BaseTextureMapping := BaseColorTextureMapping;

    ReadTexture(Material.NormalTexture,
      NormalTexture, NormalTextureMapping, TexTransforms);
    Result.NormalTexture := NormalTexture;
    Result.NormalTextureMapping := NormalTextureMapping;

    ReadTexture(Material.EmissiveTexture,
      EmissiveTexture, EmissiveTextureMapping, TexTransforms);
    Result.EmissiveTexture := EmissiveTexture;
    Result.EmissiveTextureMapping := EmissiveTextureMapping;

    ReadTexture(PbrMetallicRoughness.MetallicRoughnessTexture,
      MetallicRoughnessTexture, MetallicRoughnessTextureMapping, TexTransforms);
    Result.MetallicRoughnessTexture := MetallicRoughnessTexture;
    Result.MetallicRoughnessTextureMapping := MetallicRoughnessTextureMapping;

    ReadTexture(Material.OcclusionTexture,
      OcclusionTexture, OcclusionTextureMapping, TexTransforms);
    Result.OcclusionTexture := OcclusionTexture;
    Result.OcclusionTextureMapping := OcclusionTextureMapping;
    Result.OcclusionStrength := Material.OcclusionTexture.Strength;
  end;

  { Read glTF unlit material, see
    https://github.com/KhronosGroup/glTF/tree/master/extensions/2.0/Khronos/KHR_materials_unlit .
    Note that baseColor/Texture is converted to X3D emissiveColor/Texture. }
  function ReadUnlitMaterial(const Material: TPasGLTF.TMaterial;
    const TexTransforms: TTextureTransforms): TUnlitMaterialNode;
  var
    BaseColorFactor: TVector4;
    BaseColorTexture, NormalTexture: TAbstractTexture2DNode;
    BaseColorTextureMapping, NormalTextureMapping: String;
  begin
    BaseColorFactor := Vector4FromGltf(Material.PBRMetallicRoughness.BaseColorFactor);

    Result := TUnlitMaterialNode.Create;
    Result.EmissiveColor := BaseColorFactor.XYZ;
    Result.Transparency := 1 - BaseColorFactor.W;

    ReadTexture(Material.PBRMetallicRoughness.BaseColorTexture,
      BaseColorTexture, BaseColorTextureMapping, TexTransforms);
    Result.EmissiveTexture := BaseColorTexture;
    Result.EmissiveTextureMapping := BaseColorTextureMapping;

    { We read normal texture, even though it isn't *usually* useful for UnlitMaterial.
      But it makes sense when geometry has TextureCoordinateGenerator
      that depends on normal info.
      And both glTF and X3Dv4 allow normal texture even in case of unlit materials. }
    ReadTexture(Material.NormalTexture,
      NormalTexture, NormalTextureMapping, TexTransforms);
    Result.NormalTexture := NormalTexture;
    Result.NormalTextureMapping := NormalTextureMapping;
  end;

  function ReadAppearance(const Material: TPasGLTF.TMaterial): TGltfAppearanceNode;
  var
    AlphaMode: TAlphaMode;
    TexTransforms: TTextureTransforms;
  begin
    Result := TGltfAppearanceNode.Create(Material.Name);

    TexTransforms := TTextureTransforms.Create;
    try
      if Material.Extensions.Properties['KHR_materials_unlit'] <> nil then
        Result.Material := ReadUnlitMaterial(Material, TexTransforms)
      else
      if GltfForcePhongMaterials then
        Result.Material := ReadPhongMaterial(Material, TexTransforms)
      else
        Result.Material := ReadPhysicalMaterial(Material, TexTransforms);
      ReadMetadata(Material.Extras, Result.Material);

      // read common material properties, that make sense in case of all material type
      Result.DoubleSided := Material.DoubleSided;

      // read alpha channel treatment
      case Material.AlphaMode of
        TPasGLTF.TMaterial.TAlphaMode.Opaque: AlphaMode := amOpaque;
        TPasGLTF.TMaterial.TAlphaMode.Blend : AlphaMode := amBlend;
        TPasGLTF.TMaterial.TAlphaMode.Mask  : AlphaMode := amMask;
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('Unexpected glTF Material.AlphaMode value');
        {$endif}
      end;
      Result.AlphaMode := AlphaMode;
      Result.AlphaCutOff := Material.AlphaCutOff;

      Result.TextureTransform := TexTransforms.SingleTextureTransform;
    finally FreeAndNil(TexTransforms) end;
  end;

  procedure FixAppearances;
  var
    I: Integer;
    App: TGltfAppearanceNode;
    Mat: TPhysicalMaterialNode;
  begin
    for I := 0 to Appearances.Count - 1 do
    begin
      App := Appearances[I] as TGltfAppearanceNode;
      { When rendering unlit points and lines, glTF says to sum base and emissive color,
        see https://registry.khronos.org/glTF/specs/2.0/glTF-2.0.html#point-and-line-materials .
        X3D doesn't do this, as it is a bit weird (we also don't always have "base", our
        UnlitMaterial just has emissive).
        As a crude fix, we correct materials that we know are only used by unlit things,
        to have better emissiveColor. }
      if App.Used and (not App.UsedAsLit) and (App.Material is TPhysicalMaterialNode) then
      begin
        Mat := TPhysicalMaterialNode(App.Material);
        Mat.EmissiveColor := Mat.EmissiveColor + Mat.BaseColor;
      end;
    end;
  end;

  function AccessorTypeToStr(const AccessorType: TPasGLTF.TAccessor.TType): String;
  begin
    Result := GetEnumName(TypeInfo(TPasGLTF.TAccessor.TType), Ord(AccessorType));
  end;

  function PrimitiveModeToStr(const Mode: TPasGLTF.TMesh.TPrimitive.TMode): String;
  begin
    Result := GetEnumName(TypeInfo(TPasGLTF.TMesh.TPrimitive.TMode), Ord(Mode));
  end;

  function GetAccessor(const AccessorIndex: Integer): TPasGLTF.TAccessor;
  begin
    if AccessorIndex < Document.Accessors.Count then
      Result := Document.Accessors[AccessorIndex]
    else
    begin
      Result := nil;
      WritelnWarning('glTF', 'Missing glTF accessor (index %d, but we only have %d accessors)',
        [AccessorIndex, Document.Accessors.Count]);
    end;
  end;

  { The argument ForVertex addresses this statement of the glTF spec:
    """
    For performance and compatibility reasons, each element of
    a vertex attribute must be aligned to 4-byte boundaries
    inside bufferView
    """ }

  procedure AccessorToInt32(const AccessorIndex: Integer; const Field: TMFLong; const ForVertex: Boolean);
  var
    Accessor: TPasGLTF.TAccessor;
    A: TPasGLTFInt32DynamicArray;
    Len: Integer;
  begin
    Accessor := GetAccessor(AccessorIndex);
    if Accessor <> nil then
    begin
      A := Accessor.DecodeAsInt32Array(ForVertex);
      Len := Length(A);
      Field.Count := Len;
      if Len <> 0 then
        Move(A[0], Field.Items.List^[0], SizeOf(LongInt) * Len);
    end;
  end;

  procedure AccessorToFloat(const AccessorIndex: Integer; const Field: TMFFloat; const ForVertex: Boolean);
  var
    Accessor: TPasGLTF.TAccessor;
    A: TPasGLTFFloatDynamicArray;
    Len: Integer;
  begin
    Accessor := GetAccessor(AccessorIndex);
    if Accessor <> nil then
    begin
      A := Accessor.DecodeAsFloatArray(ForVertex);
      Len := Length(A);
      Field.Count := Len;
      if Len <> 0 then
        // Both glTF and X3D call it "Float", it is "Single" in Pascal
        Move(A[0], Field.Items.List^[0], SizeOf(Single) * Len);
    end;
  end;

  procedure AccessorToVector2(const AccessorIndex: Integer; const Field: TMFVec2f; const ForVertex: Boolean);
  var
    Accessor: TPasGLTF.TAccessor;
    A: TPasGLTF.TVector2DynamicArray;
    Len: Integer;
  begin
    Accessor := GetAccessor(AccessorIndex);
    if Accessor <> nil then
    begin
      A := Accessor.DecodeAsVector2Array(ForVertex);
      Len := Length(A);
      Field.Count := Len;
      if Len <> 0 then
        Move(A[0], Field.Items.List^[0], SizeOf(TVector2) * Len);
    end;
  end;

  procedure AccessorToVector3(const AccessorIndex: Integer; const Field: TMFVec3f; const ForVertex: Boolean);
  var
    Accessor: TPasGLTF.TAccessor;
    A: TPasGLTF.TVector3DynamicArray;
    Len: Integer;
  begin
    Accessor := GetAccessor(AccessorIndex);
    if Accessor <> nil then
    begin
      A := Accessor.DecodeAsVector3Array(ForVertex);
      Len := Length(A);
      Field.Count := Len;
      if Len <> 0 then
        Move(A[0], Field.Items.List^[0], SizeOf(TVector3) * Len);
    end;
  end;

  procedure AccessorToVector4(const AccessorIndex: Integer; const Field: TVector4List;
    const ForVertex: Boolean); overload;
  var
    Accessor: TPasGLTF.TAccessor;
    A: TPasGLTF.TVector4DynamicArray;
    Len: Integer;
  begin
    Accessor := GetAccessor(AccessorIndex);
    if Accessor <> nil then
    begin
      A := Accessor.DecodeAsVector4Array(ForVertex);
      Len := Length(A);
      Field.Count := Len;
      if Len <> 0 then
        Move(A[0], Field.List^[0], SizeOf(TVector4) * Len);
    end;
  end;

  procedure AccessorToVector4(const AccessorIndex: Integer; const Field: TMFVec4f;
    const ForVertex: Boolean); overload;
  begin
    AccessorToVector4(AccessorIndex, Field.Items, ForVertex);
  end;

  procedure AccessorToVector4Integer(const AccessorIndex: Integer; const Field: TVector4IntegerList; const ForVertex: Boolean);
  var
    Accessor: TPasGLTF.TAccessor;
    A: TPasGLTF.TInt32Vector4DynamicArray;
    Len: Integer;
  begin
    Accessor := GetAccessor(AccessorIndex);
    if Accessor <> nil then
    begin
      A := Accessor.DecodeAsInt32Vector4Array(ForVertex);
      Len := Length(A);
      Field.Count := Len;
      if Len <> 0 then
        Move(A[0], Field.List^[0], SizeOf(TVector4Integer) * Len);
    end;
  end;

  procedure AccessorToMatrix4(const AccessorIndex: Integer; const List: TMatrix4List; const ForVertex: Boolean);
  var
    Accessor: TPasGLTF.TAccessor;
    A: TPasGLTF.TMatrix4x4DynamicArray;
    Len: Integer;
  begin
    Accessor := GetAccessor(AccessorIndex);
    if Accessor <> nil then
    begin
      A := Accessor.DecodeAsMatrix4x4Array(ForVertex);
      Len := Length(A);
      List.Count := Len;
      if Len <> 0 then
        Move(A[0], List.List^[0], SizeOf(TMatrix4) * Len);
    end;
  end;

  procedure AccessorToRotation(const AccessorIndex: Integer; const Field: TMFRotation; const ForVertex: Boolean);
  var
    Accessor: TPasGLTF.TAccessor;
    A: TPasGLTF.TVector4DynamicArray;
    Len, I: Integer;
  begin
    Accessor := GetAccessor(AccessorIndex);
    if Accessor <> nil then
    begin
      A := Accessor.DecodeAsVector4Array(ForVertex);
      Len := Length(A);
      Field.Count := Len;
      // convert glTF rotation to X3D
      for I := 0 to Len - 1 do
        Field.Items.List^[I] := RotationFromGltf(A[I]);
    end;
  end;

  { Set SingleTexCoord as a texture coordinate.
    Sets up TexCoordField as a TMultiTextureCoordinateNode instance,
    in case we have multiple texture coordinates. }
  procedure SetMultiTextureCoordinate(const Geometry: TAbstractGeometryNode;
    const SingleTexCoord: TTextureCoordinateNode);
  var
    TexCoordField: TSFNode;
    MultiTexCoord: TMultiTextureCoordinateNode;
  begin
    TexCoordField := Geometry.TexCoordField;

    if TexCoordField.Value <> nil then
      { only this procedure modifies this field,
        so it has to be TMultiTextureCoordinateNode if assigned. }
      MultiTexCoord := TexCoordField.Value as TMultiTextureCoordinateNode
    else
    begin
      MultiTexCoord := TMultiTextureCoordinateNode.Create;
      TexCoordField.Value := MultiTexCoord;
    end;

    if Geometry.FindTextureMapping(SingleTexCoord.Mapping, false) <> nil then
      WritelnWarning('Texture coordinate "%s" specified multiple times for the same glTF mesh', [
        SingleTexCoord.Mapping
      ]);

    MultiTexCoord.FdTexCoord.Add(SingleTexCoord);
  end;

  procedure FlipTextureCoordinates(const TexCoord: TVector2SingleList);
  var
    I: Integer;
  begin
    for I := 0 to TexCoord.Count - 1 do
      TexCoord.List^[I].Y := 1  - TexCoord.List^[I].Y;
  end;

  function PossiblyLitGeometry(const Geometry: TAbstractGeometryNode): Boolean;
  begin
    Result := not (
      (Geometry is TPointSetNode) or
      (Geometry is TIndexedLineSetNode) or
      (Geometry is TLineSetNode)
    );
  end;

  procedure ReadPrimitive(const Primitive: TPasGLTF.TMesh.TPrimitive;
    const ParentGroup: TGroupNode);
  var
    AttributeName: TPasGLTFUTF8String;
    Shape: TShapeNode;
    Geometry: TAbstractGeometryNode;
    Coord: TCoordinateNode;
    TexCoord: TTextureCoordinateNode;
    Normal: TNormalNode;
    Tangent: TTangentNode;
    Color: TColorNode;
    ColorRGBA: TColorRGBANode;
    ColorAccessor: TPasGLTF.TAccessor;
    IndexField: TMFLong;
    Appearance: TGltfAppearanceNode;
    Tangent4D: TVector4List;
    MetadataCollision: String;
  begin
    // create X3D geometry and shape nodes
    if Primitive.Indices <> -1 then
    begin
      case Primitive.Mode of
        // TODO: We don't have indexed points in X3D.
        TPasGLTF.TMesh.TPrimitive.TMode.Points       : Geometry := TPointSetNode.CreateWithShape(Shape);
        TPasGLTF.TMesh.TPrimitive.TMode.Lines        :
          begin
            Geometry := TIndexedLineSetNode.CreateWithShape(Shape);
            TIndexedLineSetNode(Geometry).Mode := lmPair;
          end;
        TPasGLTF.TMesh.TPrimitive.TMode.LineLoop     :
          begin
            Geometry := TIndexedLineSetNode.CreateWithShape(Shape);
            TIndexedLineSetNode(Geometry).Mode := lmLoop;
          end;
        TPasGLTF.TMesh.TPrimitive.TMode.LineStrip    : Geometry := TIndexedLineSetNode.CreateWithShape(Shape);
        TPasGLTF.TMesh.TPrimitive.TMode.Triangles    : Geometry := TIndexedTriangleSetNode.CreateWithShape(Shape);
        TPasGLTF.TMesh.TPrimitive.TMode.TriangleStrip: Geometry := TIndexedTriangleStripSetNode.CreateWithShape(Shape);
        TPasGLTF.TMesh.TPrimitive.TMode.TriangleFan  : Geometry := TIndexedTriangleFanSetNode.CreateWithShape(Shape);
        else
          begin
            WritelnWarning('glTF', 'Primitive mode not implemented (in indexed mode): ' + PrimitiveModeToStr(Primitive.Mode));
            Exit;
          end;
      end;
    end else
    begin
      case Primitive.Mode of
        TPasGLTF.TMesh.TPrimitive.TMode.Points       : Geometry := TPointSetNode.CreateWithShape(Shape);
        TPasGLTF.TMesh.TPrimitive.TMode.Lines        :
          begin
            Geometry := TLineSetNode.CreateWithShape(Shape);
            TLineSetNode(Geometry).Mode := lmPair;
          end;
        TPasGLTF.TMesh.TPrimitive.TMode.LineLoop     :
          begin
            Geometry := TLineSetNode.CreateWithShape(Shape);
            TLineSetNode(Geometry).Mode := lmLoop;
          end;
        TPasGLTF.TMesh.TPrimitive.TMode.LineStrip    : Geometry := TLineSetNode.CreateWithShape(Shape);
        TPasGLTF.TMesh.TPrimitive.TMode.Triangles    : Geometry := TTriangleSetNode.CreateWithShape(Shape);
        TPasGLTF.TMesh.TPrimitive.TMode.TriangleStrip: Geometry := TTriangleStripSetNode.CreateWithShape(Shape);
        TPasGLTF.TMesh.TPrimitive.TMode.TriangleFan  : Geometry := TTriangleFanSetNode.CreateWithShape(Shape);
        else
          begin
            WritelnWarning('glTF', 'Primitive mode not implemented (in non-indexed) mode: ' + PrimitiveModeToStr(Primitive.Mode));
            Exit;
          end;
      end;
    end;

    // read indexes
    IndexField := Geometry.CoordIndexField;
    if IndexField <> nil then
    begin
      Assert(Primitive.Indices <> -1);
      AccessorToInt32(Primitive.Indices, IndexField, false);
    end;

    // parse attributes (initializing Coord, TexCoord and other such nodes)
    // TODO: ForVertex true for all, or just for POSITION?
    for AttributeName in Primitive.Attributes.Keys do
    begin
      if (AttributeName = 'POSITION') and (Geometry.CoordField <> nil) then
      begin
        Coord := TCoordinateNode.Create;
        AccessorToVector3(Primitive.Attributes[AttributeName], Coord.FdPoint, true);
        Geometry.CoordField.Value := Coord;
        Shape.BBox := TBox3D.FromPoints(Coord.FdPoint.Items);
        { Do special fix for line strip and line loop: glTF specifies just one strip/loop,
          put it in VertexCount. }
        if (Geometry is TLineSetNode) and
           (TLineSetNode(Geometry).Mode in [lmStrip, lmLoop]) then
          TLineSetNode(Geometry).SetVertexCount([Coord.FdPoint.Count]);
      end else
      if IsPrefix('TEXCOORD_', AttributeName, false) and (Geometry.TexCoordField <> nil) then
      begin
        TexCoord := TTextureCoordinateNode.Create;
        TexCoord.Mapping := AttributeName;
        AccessorToVector2(Primitive.Attributes[AttributeName], TexCoord.FdPoint, false);
        { We prefer to flip the texture, using TImageTextureNode.FlipVertically,
          and not make a (slower) flipping of texture coordinates.
          But when CastleX3dExtensions = false, we have no other choice right now but to flip them. }
        if not CastleX3dExtensions then
          FlipTextureCoordinates(TexCoord.FdPoint.Items);
        SetMultiTextureCoordinate(Geometry, TexCoord);
      end else
      if (AttributeName = 'NORMAL') and (Geometry is TAbstractComposedGeometryNode) then
      begin
        Normal := TNormalNode.Create;
        AccessorToVector3(Primitive.Attributes[AttributeName], Normal.FdVector, false);
        TAbstractComposedGeometryNode(Geometry).FdNormal.Value := Normal;
      end else
      if (AttributeName = 'COLOR_0') and (Geometry.ColorField <> nil) then
      begin
        ColorAccessor := GetAccessor(Primitive.Attributes[AttributeName]);
        if ColorAccessor.Type_ = TPasGLTF.TAccessor.TType.Vec4 then
        begin
          ColorRGBA := TColorRGBANode.Create;
          ColorRGBA.Mode := cmModulate;
          AccessorToVector4(Primitive.Attributes[AttributeName], ColorRGBA.FdColor, false);
          Geometry.ColorField.Value := ColorRGBA;
        end else
        begin
          Color := TColorNode.Create;
          Color.Mode := cmModulate;
          AccessorToVector3(Primitive.Attributes[AttributeName], Color.FdColor, false);
          Geometry.ColorField.Value := Color;
        end;
      end else
      if (AttributeName = 'TANGENT') and (Geometry is TAbstractComposedGeometryNode) then
      begin
        if CastleX3dExtensions then
        begin
          Tangent := TTangentNode.Create;
          Tangent4D := TVector4List.Create;
          try
            AccessorToVector4(Primitive.Attributes[AttributeName], Tangent4D, false);
            Tangent.SetVector4D(Tangent4D);
          finally FreeAndNil(Tangent4D) end;
          TAbstractComposedGeometryNode(Geometry).FdTangent.Value := Tangent;
        end;
      end else
      if (AttributeName = 'JOINTS_0') then
      begin
        Geometry.InternalSkinJoints := TVector4IntegerList.Create;
        AccessorToVector4Integer(Primitive.Attributes[AttributeName], Geometry.InternalSkinJoints, false);
      end else
      if (AttributeName = 'WEIGHTS_0') then
      begin
        Geometry.InternalSkinWeights := TVector4List.Create;
        AccessorToVector4(Primitive.Attributes[AttributeName], Geometry.InternalSkinWeights, false);
      end else
        WritelnLog('glTF', 'Ignoring vertex attribute ' + AttributeName + ', not implemented (for this primitive mode)');
    end;

    // determine Appearance
    if Between(Primitive.Material, 0, Appearances.Count - 1) then
      Appearance := Appearances[Primitive.Material] as TGltfAppearanceNode
    else
    begin
      Appearance := DefaultAppearance;
      if Primitive.Material <> -1 then
        WritelnWarning('glTF', 'Primitive specifies invalid material index %d',
          [Primitive.Material]);
    end;
    Appearance.Used := true;
    Appearance.UsedAsLit := Appearance.UsedAsLit or PossiblyLitGeometry(Geometry);
    Shape.Appearance := Appearance;

    // apply additional TGltfAppearanceNode parameters, specified in X3D at geometry
    Geometry.Solid := not Appearance.DoubleSided;

    if CastleX3dExtensions then
      Shape.GenerateTangents;

    MetadataCollision := ParentGroup.MetadataString['CastleCollision'];
    if MetadataCollision = 'none' then
      Shape.Collision := scNone
    else
    if MetadataCollision = 'box' then
      Shape.Collision := scBox
    else
    if (MetadataCollision = '') or (MetadataCollision = 'default') then
      Shape.Collision := scDefault
    else
      WritelnWarning('Invalid value for "CastleCollision" custom property, ignoring: %s', [MetadataCollision]);

    // add to X3D
    ParentGroup.AddChildren(Shape);
    ReadMetadata(Primitive.Extras, Shape);
  end;

  procedure ReadMesh(const Mesh: TPasGLTF.TMesh;
    const ParentGroup: TAbstractGroupingNode); overload;
  var
    Primitive: TPasGLTF.TMesh.TPrimitive;
    Group: TGroupNode;
  begin
    Group := TGroupNode.Create;
    Group.X3DName := Mesh.Name;
    ParentGroup.AddChildren(Group);
    ExportNodes.Add(Group);

    ReadMetadata(Mesh.Extras, Group);

    for Primitive in Mesh.Primitives do
      ReadPrimitive(Primitive, Group);
  end;

  procedure ReadMesh(const MeshIndex: Integer;
    const ParentGroup: TAbstractGroupingNode); overload;
  begin
    if Between(MeshIndex, 0, Document.Meshes.Count - 1) then
      ReadMesh(Document.Meshes[MeshIndex], ParentGroup)
    else
      WritelnWarning('glTF', 'Mesh index invalid: %d', [MeshIndex]);
  end;

  procedure ReadCamera(const Camera: TPasGLTF.TCamera;
    const ParentGroup: TAbstractGroupingNode); overload;
  var
    OrthoViewpoint: TOrthoViewpointNode;
    Viewpoint: TViewpointNode;
  begin
    if Camera.Type_ = TPasGLTF.TCamera.TCameraType.Orthographic then
    begin
      OrthoViewpoint := TOrthoViewpointNode.Create;
      OrthoViewpoint.X3DName := Camera.Name;
      OrthoViewpoint.Position := TVector3.Zero;
      if CastleX3dExtensions then
        OrthoViewpoint.GravityTransform := false;
      ParentGroup.AddChildren(OrthoViewpoint);

      ReadMetadata(Camera.Extras, OrthoViewpoint);

      ExportNodes.Add(OrthoViewpoint);
    end else
    begin
      Viewpoint := TViewpointNode.Create;
      Viewpoint.X3DName := Camera.Name;
      Viewpoint.Position := TVector3.Zero;
      if Camera.Perspective.YFov <> 0 then
        Viewpoint.FieldOfView := Camera.Perspective.YFov;
      if CastleX3dExtensions then
        Viewpoint.GravityTransform := false;
      ParentGroup.AddChildren(Viewpoint);

      ReadMetadata(Camera.Extras, Viewpoint);

      ExportNodes.Add(Viewpoint);
    end;
  end;

  procedure ReadCamera(const CameraIndex: Integer;
    const ParentGroup: TAbstractGroupingNode); overload;
  begin
    if Between(CameraIndex, 0, Document.Cameras.Count - 1) then
      ReadCamera(Document.Cameras[CameraIndex], ParentGroup)
    else
      WritelnWarning('glTF', 'Camera index invalid: %d', [CameraIndex]);
  end;

  procedure ReadNode(const NodeIndex: Integer; const ParentGroup: TAbstractGroupingNode);
  var
    Transform: TTransformNode;

    { Apply Node.Skin, adding a new item to SkinsToInitialize list
      and making the node collide as a box (otherwise every frame we would recalculate octree). }
    procedure ApplySkin(const Skin: TPasGLTF.TSkin);
    var
      SkinToInitialize: TSkinToInitialize;
      Shapes: TAbstractGroupingNode;
      I: Integer;
      ShapeNode: TShapeNode;
    begin
      SkinToInitialize := TSkinToInitialize.Create;
      SkinsToInitialize.Add(SkinToInitialize);
      // Shapes is the group created inside ReadMesh
      Shapes := Transform.FdChildren.InternalItems.Last as TAbstractGroupingNode;
      SkinToInitialize.Shapes := Shapes;
      SkinToInitialize.ShapesParent := Transform;
      SkinToInitialize.Skin := Skin;

      { Make shapes collide as simple boxes.
        We don't want to recalculate octree of their triangles each frame,
        and their boxes are easy, since we fill shape's bbox. }
      for I := 0 to Shapes.FdChildren.Count - 1 do
        if Shapes.FdChildren[I] is TShapeNode then
        begin
          ShapeNode := TShapeNode(Shapes.FdChildren[I]);
          ShapeNode.Collision := scBox;
        end;
    end;

  var
    Node: TPasGLTF.TNode;
    NodeMatrix: TMatrix4;
    Translation, Scale: TVector3;
    Rotation: TVector4;
    ChildNodeIndex: Integer;
  begin
    if Between(NodeIndex, 0, Document.Nodes.Count - 1) then
    begin
      Node := Document.Nodes[NodeIndex];
      NodeMatrix := Matrix4FromGltf(Node.Matrix);

      if not TMatrix4.PerfectlyEquals(NodeMatrix, TMatrix4.Identity) then
      begin
        MatrixDecompose(NodeMatrix, Translation, Rotation, Scale);
      end else
      begin
        Translation := Vector3FromGltf(Node.Translation);
        Rotation := RotationFromGltf(Node.Rotation);
        Scale := Vector3FromGltf(Node.Scale);
      end;

      Transform := TTransformNode.Create;
      Transform.X3DName := Node.Name;
      { Assign name to more easily recognize this in X3D output. }
      if Transform.X3DName = '' then
        Transform.X3DName := 'Node' + IntToStr(NodeIndex);
      Transform.Translation := Translation;
      Transform.Rotation := Rotation;
      Transform.Scale := Scale;
      ParentGroup.AddChildren(Transform);

      ReadMetadata(Node.Extras, Transform);

      if Node.Mesh <> -1 then
      begin
        ReadMesh(Node.Mesh, Transform);

        if Node.Skin <> -1 then
        begin
          if Between(Node.Skin, 0, Document.Skins.Count - 1) then
            ApplySkin(Document.Skins[Node.Skin])
          else
            WritelnWarning('glTF', 'Skin index invalid: %d', [Node.Skin]);
        end;
      end;

      if Node.Camera <> -1 then
        ReadCamera(Node.Camera, Transform);

      Lights.ReadNode(Node, Transform);

      for ChildNodeIndex in Node.Children do
        ReadNode(ChildNodeIndex, Transform);

      // add to Nodes list
      Nodes.Count := Max(Nodes.Count, NodeIndex + 1);
      if Nodes[NodeIndex] <> nil then
        WritelnWarning('glTF', 'Node %d read multiple times (impossible if glTF is a strict tree)', [NodeIndex])
      else
        Nodes[NodeIndex] := Transform;
    end else
      WritelnWarning('glTF', 'Node index invalid: %d', [NodeIndex]);
  end;

  procedure ReadScene(const SceneIndex: Integer; const ParentGroup: TAbstractGroupingNode);
  var
    Scene: TPasGLTF.TScene;
    NodeIndex: Integer;
  begin
    if Between(SceneIndex, 0, Document.Scenes.Count - 1) then
    begin
      Scene := Document.Scenes[SceneIndex];
      for NodeIndex in Scene.Nodes do
        ReadNode(NodeIndex, ParentGroup);
      AnimationSampler.TransformNodesRoots := Scene.Nodes;
    end else
      WritelnWarning('glTF', 'Scene index invalid: %d', [SceneIndex]);
  end;

  function ReadSampler(const Sampler: TPasGLTF.TAnimation.TSampler;
    const Node: TTransformNode;
    const Path: TGltfSamplerPath;
    const TimeSensor: TTimeSensorNode;
    const ParentGroup: TAbstractGroupingNode;
    out Duration: TFloatTime): TAbstractInterpolatorNode;
  var
    InterpolatePosition: TPositionInterpolatorNode;
    InterpolateOrientation: TOrientationInterpolatorNode;
    Interpolator: TAbstractInterpolatorNode;
    InterpolatorOutputEvent: TX3DEvent;
    TargetField: TX3DField;
    I: Integer;
  begin
    // silence Delphi warnings
    InterpolatePosition := nil;
    InterpolateOrientation := nil;

    case Path of
      gsTranslation, gsScale:
        begin
          InterpolatePosition := TPositionInterpolatorNode.Create;
          Interpolator := InterpolatePosition;
          InterpolatorOutputEvent := InterpolatePosition.EventValue_changed;
          AccessorToVector3(Sampler.Output, InterpolatePosition.FdKeyValue, false);
          case Path of
            gsTranslation: TargetField := Node.FdTranslation;
            gsScale      : TargetField := Node.FdScale;
            else raise EInternalError.Create('ReadSampler vector3 - Path?');
          end;
        end;
      gsRotation:
        begin
          InterpolateOrientation := TOrientationInterpolatorNode.Create;
          Interpolator := InterpolateOrientation;
          InterpolatorOutputEvent := InterpolateOrientation.EventValue_changed;
          AccessorToRotation(Sampler.Output, InterpolateOrientation.FdKeyValue, false);
          TargetField := Node.FdRotation;
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('ReadSampler - Path?');
      {$endif}
    end;

    { Put both Node.X3DName and TargetField.X3DName into Interpolator.X3DName.
      We create new interpolator for each target node (Transform) and field
      (like translation, rotation...) so we want to generate unique names for them
      (if animation names in glTF were unique). }
    Interpolator.X3DName := 'Animate_' + TimeSensor.X3DName + '_' + Node.X3DName + '_' + TargetField.X3DName;

    AccessorToFloat(Sampler.Input, Interpolator.FdKey, false);
    if Interpolator.FdKey.Count <> 0 then
      Duration := Interpolator.FdKey.Items.Last
    else
      Duration := 0;

    ParentGroup.AddChildren(Interpolator);

    ParentGroup.AddRoute(TimeSensor.EventFraction_changed, Interpolator.EventSet_fraction);
    ParentGroup.AddRoute(InterpolatorOutputEvent, TargetField);

    Result := Interpolator;

    // take into account Interpolation
    case Sampler.Interpolation of
      TPasGLTF.TAnimation.TSampler.TSamplerType.Linear: ; // nothing to do
      TPasGLTF.TAnimation.TSampler.TSamplerType.Step:
        Interpolator.Interpolation := inStep;
      TPasGLTF.TAnimation.TSampler.TSamplerType.CubicSpline:
        begin
          // May spam too much. Assume that our current approximation is good enough.
          // WritelnWarning('Animation interpolation "CubicSpline" not supported yet, approximating by "Linear"');
          case Path of
            gsTranslation, gsScale:
              begin
                if InterpolatePosition.FdKeyValue.Count <>
                   InterpolatePosition.FdKey.Count * 3 then
                begin
                  WritelnWarning('For "CubicSpline", expected 3 output values for each input time, got %d for %d', [
                    InterpolatePosition.FdKeyValue.Count,
                    InterpolatePosition.FdKey.Count
                  ]);
                  Exit;
                end;
                for I := 0 to InterpolatePosition.FdKeyValue.Count div 3 - 1 do
                  InterpolatePosition.FdKeyValue.Items[I] :=
                    InterpolatePosition.FdKeyValue.Items[3 * I + 1];
                InterpolatePosition.FdKeyValue.Count := InterpolatePosition.FdKeyValue.Count div 3;
              end;
            gsRotation:
              begin
                if InterpolateOrientation.FdKeyValue.Count <>
                   InterpolateOrientation.FdKey.Count * 3 then
                begin
                  WritelnWarning('For "CubicSpline", expected 3 output values for each input time, got %d for %d', [
                    InterpolateOrientation.FdKeyValue.Count,
                    InterpolateOrientation.FdKey.Count
                  ]);
                  Exit;
                end;
                for I := 0 to InterpolateOrientation.FdKeyValue.Count div 3 - 1 do
                  InterpolateOrientation.FdKeyValue.Items[I] :=
                    InterpolateOrientation.FdKeyValue.Items[3 * I + 1];
                InterpolateOrientation.FdKeyValue.Count := InterpolateOrientation.FdKeyValue.Count div 3;
              end;
            {$ifndef COMPILER_CASE_ANALYSIS}
            else raise EInternalError.Create('ReadSampler - Path?');
            {$endif}
          end;
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else
        begin
          WritelnWarning('Given animation interpolation is not supported');
        end;
      {$endif}
    end;
  end;

  procedure ReadAnimation(const Animation: TPasGLTF.TAnimation; const ParentGroup: TAbstractGroupingNode);
  var
    TimeSensor: TTimeSensorNode;
    Channel: TPasGLTF.TAnimation.TChannel;
    Sampler: TPasGLTF.TAnimation.TSampler;
    Node: TTransformNode;
    Duration, MaxDuration: TFloatTime;
    Interpolator: TAbstractInterpolatorNode;
    NodeIndex, I: Integer;
    Anim: TAnimation;
    InterpolatorRec: TInterpolator;
    Path: TGltfSamplerPath;
  begin
    Anim := TAnimation.Create;
    Animations.Add(Anim);

    TimeSensor := TTimeSensorNode.Create;
    if Animation.Name = '' then
      { Needs a name, otherwise TCastleSceneCore.AnimationsList would ignore it. }
      TimeSensor.X3DName := 'unnamed'
    else
      TimeSensor.X3DName := Animation.Name;
    ParentGroup.AddChildren(TimeSensor);
    Anim.TimeSensor := TimeSensor;

    MaxDuration := 0;
    for Channel in Animation.Channels do
    begin
      NodeIndex := Channel.Target.Node;

      // glTF spec says "When node isn't defined, channel should be ignored"
      if NodeIndex = -1 then
        Continue;

      if not (Between(NodeIndex, 0, Nodes.Count - 1) and (Nodes[NodeIndex] <> nil)) then
      begin
        WritelnWarning('Node index %d indicated by animation %s was not imported', [
          NodeIndex,
          TimeSensor.X3DName
        ]);
        Continue;
      end;

      Node := Nodes[NodeIndex] as TTransformNode;

      // read Sampler
      if not Between(Channel.Sampler, 0, Animation.Samplers.Count - 1) then
      begin
        WritelnWarning('Invalid animation "%s" sampler index %d', [
          TimeSensor.X3DName,
          Channel.Sampler
        ]);
        Continue;
      end;

      Sampler := Animation.Samplers[Channel.Sampler];

      // read channel Path
      if Channel.Target.Path = 'translation' then
        Path := gsTranslation
      else
      if Channel.Target.Path = 'rotation' then
        Path := gsRotation
      else
      if Channel.Target.Path = 'scale' then
        Path := gsScale
      else
      begin
        WritelnWarning('Animating "%s" not supported', [Channel.Target.Path]);
        Continue;
      end;

      // call ReadSampler with all information
      Interpolator := ReadSampler(Sampler, Node, Path, TimeSensor, ParentGroup, Duration);

      // extend Anim.Interpolators list
      InterpolatorRec.Node := Interpolator;
      InterpolatorRec.Target := Node;
      InterpolatorRec.Path := Path;
      Anim.Interpolators.Add(InterpolatorRec);

      MaxDuration := Max(MaxDuration, Duration);
    end;

    // adjust TimeSensor duration, scale the keys in all Interpolators to be in 0..1 range
    if MaxDuration <> 0 then
    begin
      TimeSensor.CycleInterval := MaxDuration;
      for I := 0 to Anim.Interpolators.Count - 1 do
      begin
        Interpolator := Anim.Interpolators[I].Node;
        Interpolator.FdKey.Items.MultiplyAll(1 / MaxDuration);
      end;
    end;
  end;

  { Gather all key times (in 0..1 range) from Interpolators, place them in AllKeys.
    If you have animation that uses multiple interpolators,
    then this routine calculates *all* key points within this animation. }
  procedure GatherAnimationKeysToSample(const AllKeys: TSingleList;
    const Interpolators: TInterpolatorList);
  var
    I: Integer;
    Interpolator: TAbstractInterpolatorNode;
  begin
    AllKeys.Clear;
    for I := 0 to Interpolators.Count - 1 do
    begin
      Interpolator := Interpolators[I].Node;
      AllKeys.AddRange(Interpolator.FdKey.Items);
    end;
    AllKeys.SortAndRemoveDuplicates;
  end;

  { Sample animation Anim at time TimeFraction (in 0..1 range)
    to determine how does a skin look like at this moment of time.
    OriginalCoords contains original (not animated) coords.
    To the AnimatedCoords, we will add OriginalCoords.Count vertexes.

    We also add to AnimatedNormals if they are <> nil.
    Both OriginalNormals and AnimatedNormals must be nil or both must be <> nil.

    Similarly, we also add AnimatedTangents if they are <> nil.
    And both OriginalTangents and AnimatedTangents must be nil or both must be <> nil.
     }
  procedure SampleSkinAnimation(const Anim: TAnimation; const KeyIndex: Integer;
    const TimeFraction: Single;
    const OriginalCoords, AnimatedCoords: TVector3List;
    const OriginalNormals, AnimatedNormals: TVector3List;
    const OriginalTangents, AnimatedTangents: TVector3List;
    const Joints: TX3DNodeList; const JointsGltf: TPasGLTF.TSkin.TJoints;
    const InverseBindMatrices: TMatrix4List;
    const SkeletonRootIndex: Integer;
    const MeshJoints: TVector4IntegerList;
    const MeshWeights: TVector4List);
  var
    I: Integer;
    SkinMatrix, SkeletonRootInverse: TMatrix4;
    VertexJoints: TVector4Integer;
    VertexWeights: TVector4;
  begin
    Assert((AnimatedNormals = nil) = (OriginalNormals = nil));
    Assert((OriginalNormals = nil) or (OriginalNormals.Count = OriginalCoords.Count));
    Assert((AnimatedTangents = nil) = (OriginalTangents = nil));
    Assert((OriginalTangents = nil) or (OriginalTangents.Count = OriginalCoords.Count));

    AnimationSampler.Animation := Anim;
    AnimationSampler.SetTime(TimeFraction);

    if SkeletonRootIndex <> -1 then
      SkeletonRootInverse := AnimationSampler.Transformations.List^[SkeletonRootIndex].InverseTransform
    else
      SkeletonRootInverse := TMatrix4.Identity;

    { For each Joint, we calculate JointMatrix following
      https://www.slideshare.net/Khronos_Group/gltf-20-reference-guide }
    for I := 0 to Joints.Count - 1 do
      JointMatrix[I] := SkeletonRootInverse *
        AnimationSampler.Transformations.List^[JointsGltf[I]].Transform *
        InverseBindMatrices[I];

    { For each vertex, calculate SkinMatrix as linear combination of JointMatrix[...]
      for all joints indicated by MeshJoints values for this vertex.
      TODO: Support JOINTS_1, WEIGHTS_1 etc. }
    for I := 0 to OriginalCoords.Count - 1 do
    begin
      VertexWeights := MeshWeights[I];
      VertexJoints := MeshJoints[I];
      if VertexWeights.IsPerfectlyZero then
      begin
        { Happens with glTF files generated by Blender.
          This is not correct (glTF spec says that weights should sum to 1.0).
          Solution that works: Transform it with weight 1 by the joint number 0
          (relying that Blender put root joint at this position). See
          https://github.com/KhronosGroup/glTF/issues/1213
          https://github.com/KhronosGroup/glTF-Blender-IO/issues/308
          https://github.com/KhronosGroup/glTF-Blender-IO/issues/308#issuecomment-531355129
            """it's not exactly a satisfying fix, but in practice using 1, 0, 0, 0 when the weights would otherwise be zero has avoided these issues in threejs."""
          https://github.com/KhronosGroup/glTF/pull/1352
          https://github.com/Franck-Dernoncourt/NeuroNER/issues/91 }
        SkinMatrix := JointMatrix.List^[0];
      end else
      begin
        SkinMatrix :=
          JointMatrix.List^[VertexJoints.X] * VertexWeights.X +
          JointMatrix.List^[VertexJoints.Y] * VertexWeights.Y +
          JointMatrix.List^[VertexJoints.Z] * VertexWeights.Z +
          JointMatrix.List^[VertexJoints.W] * VertexWeights.W;
      end;
      AnimatedCoords.List^[KeyIndex * OriginalCoords.Count + I] := SkinMatrix.MultPoint(OriginalCoords[I]);
      if AnimatedNormals <> nil then
        AnimatedNormals.List^[KeyIndex * OriginalNormals.Count + I] := SkinMatrix.MultDirection(OriginalNormals[I]);
      if AnimatedTangents <> nil then
        AnimatedTangents.List^[KeyIndex * OriginalTangents.Count + I] := SkinMatrix.MultDirection(OriginalTangents[I]);
    end;
  end;

  { When animation TimeSensor starts, set Shape.BBox using X3D routes. }
  procedure SetBBoxWhenAnimationStarts(const TimeSensor: TTimeSensorNode;
    const Shape: TShapeNode; const BBox: TBox3D;
    const ParentGroup: TAbstractGroupingNode);
  var
    ValueTrigger: TValueTriggerNode;
    Center, Size: TVector3;
    F: TX3DField;
  begin
    BBox.ToCenterSize(Center, Size);

    ValueTrigger := TValueTriggerNode.Create;
    ValueTrigger.X3DName := 'ValueTrigger_setBBox_' + TimeSensor.X3DName;
    ParentGroup.AddChildren(ValueTrigger);
    ParentGroup.AddRoute(TimeSensor.EventIsActive, ValueTrigger.EventTrigger);

    F := TSFVec3f.Create(nil, true, 'bboxCenter', Center);
    ValueTrigger.AddCustomField(F);
    ParentGroup.AddRoute(F, Shape.FdBboxCenter);

    F := TSFVec3f.Create(nil, true, 'bboxSize', Size);
    ValueTrigger.AddCustomField(F);
    ParentGroup.AddRoute(F, Shape.FdBboxSize);
  end;

  { Calculate skin interpolator nodes to deform this one shape.

    Note that ParentGroup can be really any grouping node,
    we add there only interpolators and routes, it doesn't matter where this node is. }
  procedure CalculateSkinInterpolators(const Shape: TShapeNode;
    const Joints: TX3DNodeList; const JointsGltf: TPasGLTF.TSkin.TJoints;
    const InverseBindMatrices: TMatrix4List;
    const SkeletonRoot: TAbstractGroupingNode; const SkeletonRootIndex: Integer;
    const ParentGroup: TAbstractGroupingNode);
  var
    CoordField: TSFNode;
    Coord: TCoordinateNode;
    Normal: TNormalNode;
    Tangent: TTangentNode;
    Anim: TAnimation;
    CoordInterpolator: TCoordinateInterpolatorNode;
    NormalInterpolator: TCoordinateInterpolatorNode;
    TangentInterpolator: TCoordinateInterpolatorNode;
    I: Integer;
    OriginalNormals, AnimatedNormals: TVector3List;
    OriginalTangents, AnimatedTangents: TVector3List;
    MemoryTaken: Int64;
  begin
    CoordField := Shape.Geometry.CoordField;
    if CoordField = nil then
    begin
      WritelnWarning('Cannot animate using skin geometry %s, it does not have coordinates', [
        Shape.Geometry.NiceName
      ]);
      Exit;
    end;

    if not (CoordField.Value is TCoordinateNode) then
    begin
      WritelnWarning('Cannot animate using skin geometry %s, the coordinates are not expressed as Coordinate node', [
        Shape.Geometry.NiceName
      ]);
      Exit;
    end;
    Coord := CoordField.Value as TCoordinateNode;

    // calculate Normal
    Normal := nil;
    if (Shape.Geometry.NormalField <> nil) and
       (Shape.Geometry.NormalField.Value is TNormalNode) then
    begin
      Normal := TNormalNode(Shape.Geometry.NormalField.Value);
      // SampleSkinAnimation assumes that normals and coords counts are equal
      if Normal.FdVector.Count <> Coord.FdPoint.Count then
      begin
        WritelnWarning('When animating using skin geometry %s, coords and normals counts different', [
          Shape.Geometry.NiceName
        ]);
        Normal := nil;
      end;
    end else
    begin
      if (Shape.Material is TMaterialNode) or
         (Shape.Material is TPhysicalMaterialNode) then
        WritelnWarning('TODO: Normal vectors are not provided for a skinned geometry (using lit material), and in effect the resulting animation will be slow as we''ll recalculate normals more often than necessary. ' + 'For now it is adviced to generate glTF with normals included for skinned meshes.');
    end;

    // calculate Tangent
    Tangent := nil;
    if (Shape.Geometry.TangentField <> nil) and
       (Shape.Geometry.TangentField.Value is TTangentNode) then
    begin
      Tangent := TTangentNode(Shape.Geometry.TangentField.Value);
      // SampleSkinAnimation assumes that tangents and coords counts are equal
      if Tangent.FdVector.Count <> Coord.FdPoint.Count then
      begin
        WritelnWarning('When animating using skin geometry %s, coords and tangents counts different', [
          Shape.Geometry.NiceName
        ]);
        Tangent := nil;
      end;
    end else
    begin
      if ( (Shape.Material is TMaterialNode) or
           (Shape.Material is TPhysicalMaterialNode) ) and
         ((Shape.Material as TAbstractOneSidedMaterialNode).NormalTexture <> nil) then
        WritelnWarning('TODO: Tangent vectors are not provided for a skinned geometry (using lit material with normalmap), and in effect the resulting animation will be slow as we''ll recalculate tangents more often than necessary. ' + 'For now it is adviced to generate glTF with tangents included for skinned meshes.');
    end;

    if (Shape.Geometry.InternalSkinJoints = nil) or
       (Shape.Geometry.InternalSkinWeights = nil) then
    begin
      WritelnWarning('Cannot animate using skin geometry %s, no JOINTS_0 and WEIGHTS_0 information in the mesh', [
        Shape.Geometry.NiceName
      ]);
      Exit;
    end;

    for Anim in Animations do
    begin
      CoordInterpolator := TCoordinateInterpolatorNode.Create;
      CoordInterpolator.X3DName := 'SkinCoordInterpolator_' + Anim.TimeSensor.X3DName;
      GatherAnimationKeysToSample(CoordInterpolator.FdKey.Items, Anim.Interpolators);
      { Assign count, avoids later reallocating memory when adding vectors (slow),
        and avoids Capacity >> Count (wasted memory).
        This is important on large models.
        Testcase: mouse_multiple,
        - memory use: 180 MB vs 140 MB on each animation of dancing
        - loading time: 14 vs 10 sec total. }
      CoordInterpolator.FdKeyValue.Count := CoordInterpolator.FdKey.Count * Coord.FdPoint.Count;

      ParentGroup.AddChildren(CoordInterpolator);
      ParentGroup.AddRoute(Anim.TimeSensor.EventFraction_changed, CoordInterpolator.EventSet_fraction);
      ParentGroup.AddRoute(CoordInterpolator.EventValue_changed, Coord.FdPoint);

      if Normal <> nil then
      begin
        NormalInterpolator := TCoordinateInterpolatorNode.Create;
        NormalInterpolator.X3DName := 'SkinNormalInterpolator_' + Anim.TimeSensor.X3DName;
        //GatherAnimationKeysToSample(NormalInterpolator.FdKey.Items, Anim.Interpolators);
        // faster:
        NormalInterpolator.FdKey.Assign(CoordInterpolator.FdKey);
        NormalInterpolator.FdKeyValue.Count := NormalInterpolator.FdKey.Count * Normal.FdVector.Count;

        ParentGroup.AddChildren(NormalInterpolator);
        ParentGroup.AddRoute(Anim.TimeSensor.EventFraction_changed, NormalInterpolator.EventSet_fraction);
        ParentGroup.AddRoute(NormalInterpolator.EventValue_changed, Normal.FdVector);

        OriginalNormals := Normal.FdVector.Items;
        AnimatedNormals := NormalInterpolator.FdKeyValue.Items;
      end else
      begin
        NormalInterpolator := nil;
        OriginalNormals := nil;
        AnimatedNormals := nil;
      end;

      if Tangent <> nil then
      begin
        TangentInterpolator := TCoordinateInterpolatorNode.Create;
        TangentInterpolator.X3DName := 'SkinTangentInterpolator_' + Anim.TimeSensor.X3DName;
        //GatherAnimationKeysToSample(TangentInterpolator.FdKey.Items, Anim.Interpolators);
        // faster:
        TangentInterpolator.FdKey.Assign(CoordInterpolator.FdKey);
        TangentInterpolator.FdKeyValue.Count := TangentInterpolator.FdKey.Count * Tangent.FdVector.Count;

        ParentGroup.AddChildren(TangentInterpolator);
        ParentGroup.AddRoute(Anim.TimeSensor.EventFraction_changed, TangentInterpolator.EventSet_fraction);
        ParentGroup.AddRoute(TangentInterpolator.EventValue_changed, Tangent.FdVector);

        OriginalTangents := Tangent.FdVector.Items;
        AnimatedTangents := TangentInterpolator.FdKeyValue.Items;
      end else
      begin
        TangentInterpolator := nil;
        OriginalTangents := nil;
        AnimatedTangents := nil;
      end;

      for I := 0 to CoordInterpolator.FdKey.Items.Count - 1 do
      begin
        SampleSkinAnimation(Anim, I, CoordInterpolator.FdKey.Items[I],
          Coord.FdPoint.Items, CoordInterpolator.FdKeyValue.Items,
          OriginalNormals, AnimatedNormals,
          OriginalTangents, AnimatedTangents,
          Joints, JointsGltf, InverseBindMatrices,
          SkeletonRootIndex,
          Shape.Geometry.InternalSkinJoints,
          Shape.Geometry.InternalSkinWeights);
      end;

      MemoryTaken := CoordInterpolator.FdKeyValue.Items.Capacity * SizeOf(TVector3);
      if NormalInterpolator <> nil then
        MemoryTaken := MemoryTaken + NormalInterpolator.FdKeyValue.Items.Capacity * SizeOf(TVector3);
      if TangentInterpolator <> nil then
        MemoryTaken := MemoryTaken + TangentInterpolator.FdKeyValue.Items.Capacity * SizeOf(TVector3);
      if MemoryTaken > 10 * 1024 * 1024 then // report only when memory usage > 10 MB
        WritelnLog('glTF', 'Memory occupied by precalculating "%s" animation: %s', [
          Anim.TimeSensor.X3DName,
          SizeToStr(MemoryTaken)
        ]);

      { We want to use Shape.BBox for optimization (to avoid recalculating bbox).
        Simple version:

          Shape.BBox := Shape.BBox + TBox3D.FromPoints(CoordInterpolator.FdKeyValue.Items);

        But it's more efficient to set bbox for specific animation
        once the animation starts.
        It also looks a bit more intuitive when you view bbox
        (otherwise you would see a large bbox accounting for *all* animations,
        but with mesh transformed with current animation, testcase: Bee, Monster).

        This matters, because bbox is also used for collisions.
        E.g. knight in fps_game when Walking should not have huge bbox
        because of Dying animation. }
      SetBBoxWhenAnimationStarts(Anim.TimeSensor, Shape,
        TBox3D.FromPoints(CoordInterpolator.FdKeyValue.Items),
        ParentGroup);
    end;
  end;

  { Apply Skin to deform shapes list. }
  procedure ReadSkin(const SkinToInitialize: TSkinToInitialize;
    const ParentGroup: TAbstractGroupingNode);
  var
    SkeletonRootIndex: Integer;
    SkeletonRoot: TAbstractGroupingNode;
    Joints: TX3DNodeList;
    InverseBindMatrices: TMatrix4List;
    I: Integer;
    Skin: TPasGLTF.TSkin;
    Shapes: TAbstractGroupingNode;
    ShapeNode: TShapeNode;
  begin
    Skin := SkinToInitialize.Skin;
    Shapes := SkinToInitialize.Shapes;

    SkeletonRootIndex := Skin.Skeleton;
    if SkeletonRootIndex = -1 then
      SkeletonRoot := Result // root node created by LoadGltf
    else
    begin
      if not Between(SkeletonRootIndex, 0, Nodes.Count - 1) then
      begin
        WritelnWarning('Skin "%s" specifies invalid skeleton root node index %d', [
          Skin.Name,
          SkeletonRootIndex
        ]);
        Exit;
      end;
      SkeletonRoot := Nodes[SkeletonRootIndex] as TAbstractGroupingNode;
    end;

    // first nil local variables, to reliably do try..finally that includes them all
    Joints := nil;
    InverseBindMatrices := nil;

    try
      Joints := TX3DNodeList.Create(false);
      Joints.Count := Skin.Joints.Count;
      for I := 0 to Skin.Joints.Count - 1 do
      begin
        if not Between(Skin.Joints[I], 0, Nodes.Count - 1) then
        begin
          WritelnWarning('Skin "%s" specifies invalid joint index %d', [
            Skin.Name,
            Skin.Joints[I]
          ]);
          Exit;
        end;
        Joints[I] := Nodes[Skin.Joints[I]];
      end;

      InverseBindMatrices := TMatrix4List.Create;
      AccessorToMatrix4(Skin.InverseBindMatrices, InverseBindMatrices, false);

      if Joints.Count <> InverseBindMatrices.Count then
      begin
        WritelnWarning('Joints and InverseBindMatrices counts differ for skin "%s": %d and %d', [
          Skin.Name,
          Joints.Count,
          InverseBindMatrices.Count
        ]);
        Exit;
      end;

      JointMatrix.Count := Joints.Count;

      { To satisfy glTF requirements
        """
        Client implementations should apply only the transform of the skeleton root
        node to the skinned mesh while ignoring the transform of the skinned mesh node.
        """
        Just reparent the meshes under skeleton root.

        Testcase: demo-models/blender/skinned_animation/skinned_anim.glb . }
      if SkinToInitialize.ShapesParent <> SkeletonRoot then
      begin
        SkeletonRoot.AddChildren(Shapes);
        SkinToInitialize.ShapesParent.RemoveChildren(Shapes);
        SkinToInitialize.ShapesParent := SkeletonRoot;
      end;

      for I := 0 to Shapes.FdChildren.Count - 1 do
        if Shapes.FdChildren[I] is TShapeNode then
        begin
          ShapeNode := TShapeNode(Shapes.FdChildren[I]);
          CalculateSkinInterpolators(ShapeNode,
            Joints, Skin.Joints, InverseBindMatrices,
            SkeletonRoot, SkeletonRootIndex, ParentGroup);
        end;
    finally
      FreeAndNil(Joints);
      FreeAndNil(InverseBindMatrices);
    end;
  end;

  { Read glTF skins, which result in CoordinateInterpolator nodes
    attached to shapes.
    Must be called after Nodes and SkinsToInitialize are ready, so after ReadNodes. }
  procedure ReadSkins(const ParentGroup: TAbstractGroupingNode);
  var
    SkinToInitialize: TSkinToInitialize;
  begin
    // one-time initialization of structures to process skin
    AnimationSampler.TransformNodes := Nodes;
    AnimationSampler.TransformNodesGltf := Document.Nodes;

    for SkinToInitialize in SkinsToInitialize do
      ReadSkin(SkinToInitialize, ParentGroup);
  end;

  { EXPORT nodes, so that using glTF animations in X3D is possible, like on
    demo-models/blender/skinned_animation/skinned_anim_run_animations_from_x3d.x3dv }
  procedure DoExportNodes;
  var
    N: TX3DNode;
    Anim: TAnimation;
  begin
    for N in ExportNodes do
      if N.X3DName <> '' then
        Result.ExportNode(N);

    for N in Appearances do
      if N.X3DName <> '' then
        Result.ExportNode(N);

    for N in Nodes do
      if (N <> nil) and (N.X3DName <> '') then
        Result.ExportNode(N);

    for Anim in Animations do
    begin
      N := Anim.TimeSensor;
      if N.X3DName <> '' then
        Result.ExportNode(N);
    end;
  end;

var
  Material: TPasGLTF.TMaterial;
  Animation: TPasGLTF.TAnimation;
begin
  Result := TX3DRootNode.Create('', BaseUrl);
  try
    // Set to nil local variables, to avoid nested try..finally..end construction
    Document := nil;
    DefaultAppearance := nil;
    Appearances := nil;
    Nodes := nil;
    ExportNodes := nil;
    SkinsToInitialize := nil;
    Animations := nil;
    AnimationSampler := nil;
    JointMatrix := nil;
    Lights := nil;
    try
      Document := TMyGltfDocument.Create(Stream, BaseUrl);
      SkinsToInitialize := TSkinToInitializeList.Create(true);
      Animations := TAnimationList.Create(true);
      AnimationSampler := TAnimationSampler.Create;
      JointMatrix := TMatrix4List.Create;
      Lights := TPunctualLights.Create;
      ExportNodes := TX3DNodeList.Create(false);

      ReadHeader;
      Lights.ReadHeader(Document);

      { Initialize DefaultAppearance.
        Testcase: floor of ~/sources/castle-engine/demo-models/gltf/punctual_lights/test_lights.gltf,
        it should use PBR (not be affected by PhongShading attribute). }
      DefaultAppearance := TGltfAppearanceNode.Create;
      DefaultAppearance.Material := TPhysicalMaterialNode.Create;
      DefaultAppearance.DoubleSided := false;

      // read appearances (called "materials" in glTF; in X3D "material" is something smaller)
      Appearances := TX3DNodeList.Create(false);
      for Material in Document.Materials do
        Appearances.Add(ReadAppearance(Material));

      // read main scene
      Nodes := TX3DNodeList.Create(false);
      if Document.Scene <> -1 then
        ReadScene(Document.Scene, Result)
      else
      begin
        WritelnWarning('glTF does not specify a default scene to render. We will import the 1st scene, if available.');
        ReadScene(0, Result);
      end;

      // once appearances Used, UsedAsLit are set, fix them
      FixAppearances;

      // read animations
      for Animation in Document.Animations do
        ReadAnimation(Animation, Result);
      ReadSkins(Result);
      DoExportNodes;
    finally
      FreeAndNil(JointMatrix);
      FreeAndNil(Animations);
      FreeAndNil(SkinsToInitialize);
      FreeAndNil(AnimationSampler);
      FreeIfUnusedAndNil(DefaultAppearance);
      X3DNodeList_FreeUnusedAndNil(Appearances);
      { Note that some Nodes[...] items may be nil.

        While in glTF there are no gaps (the nodes are a list without gaps),
        but a particular Document.Scene may refer only to a subset of nodes,
        and our ReadNode reads them recursively.
        Unused nodes (not referred by Document.Scene) are left unprocessed,
        and their Nodes[...] remains nil.

        Still, X3DNodeList_FreeUnusedAndNil guarantees to handle it.
        Testcase: GLB from https://www.kenney.nl/assets/city-kit-suburban . }
      X3DNodeList_FreeUnusedAndNil(Nodes);
      FreeAndNil(ExportNodes);
      FreeAndNil(Lights);
      FreeAndNil(Document);
    end;
  except FreeAndNil(Result); raise end;
end;

{$ifndef FPC}
{$POINTERMATH OFF}
{$endif}

end.
