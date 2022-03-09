{
  Copyright 2008-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Nodes that are defined by InstantReality extensions, see
  [http://instant-reality.com/] and in particular the specifications on
  [http://instant-reality.com/documentation/nodetype/]. }

{$ifdef read_interface}
  { Transform children by an explicit 4x4 matrix.

    This is an extension, not present in the X3D standard.
    See https://castle-engine.io/x3d_extensions.php#section_ext_matrix_transform .
    It is compatible with InstantReality,
    see http://instant-reality.com/documentation/nodetype/MatrixTransform/ . }
  TMatrixTransformNode = class(TAbstractX3DGroupingNode)
  strict private
    type
      TMatrixTransformNodeTransformFunctionality = class(TTransformFunctionality)
      strict private
        FParent: TMatrixTransformNode;
      public
        constructor Create(const AParent: TMatrixTransformNode);
        property Parent: TMatrixTransformNode read FParent;
        procedure ApplyTransform(var Transformation: TTransformation); override;
      end;
    var
      FTransformFunctionality: TMatrixTransformNodeTransformFunctionality;
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    class function ForVRMLVersion(const Version: TX3DVersion): boolean; override;
    function TransformationChange: TNodeTransformationChange; override;
  {$I auto_generated_node_helpers/x3dnodes_matrixtransform.inc}
  end;
  TMatrixTransformNode_2 = TMatrixTransformNode;

  { Teapot geometry, useful for testing purposes.
    See https://castle-engine.io/x3d_extensions.php#section_ext_teapot . }
  TTeapotNode = class(TAbstractGeometryNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;

    function TexCoordField: TSFNode; override;
    function AutoGenerate3DTexCoords: boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_teapot.inc}
  end;

  { X3D events logging.
    See https://castle-engine.io/x3d_extensions.php#section_ext_logger . }
  TLoggerNode = class(TAbstractChildNode)
  strict private
    LogFileStream: TStream;
    procedure EventWriteReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    destructor Destroy; override;
    {$I auto_generated_node_helpers/x3dnodes_logger.inc}
  end;

  TConverterNode = class(TAbstractChildNode)
  strict private
    procedure EventInReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    { Event in } { }
    strict private FEventIn: TXFAnyEvent;
    public property EventIn: TXFAnyEvent read FEventIn;

    { Event out } { }
    strict private FEventOut: TXFAnyEvent;
    public property EventOut: TXFAnyEvent read FEventOut;
  end;

  { Texture with contents created by rendering the scene from a specific
    viewpoint. }
  TRenderedTextureNode = class(TAbstractSingleTextureNode)
  strict private
    FGeneratedTextureHandler: TGeneratedTextureHandler;
  public
    procedure CreateNode; override;
    destructor Destroy; override;

    class function ClassX3DType: string; override;

    property GeneratedTextureHandler: TGeneratedTextureHandler
      read FGeneratedTextureHandler;

    strict private FFdViewpoint: TSFNode;
    public property FdViewpoint: TSFNode read FFdViewpoint;

    strict private FFdBackground: TSFNode;
    public property FdBackground: TSFNode read FFdBackground;

    strict private FFdFog: TSFNode;
    public property FdFog: TSFNode read FFdFog;

    strict private FFdEnvironment: TSFNode;
    public property FdEnvironment: TSFNode read FFdEnvironment;

    strict private FFdScene: TSFNode;
    public property FdScene: TSFNode read FFdScene;

    strict private FFdDimensions: TMFInt32;
    public property FdDimensions: TMFInt32 read FFdDimensions;

    strict private FFdDepthMap: TMFBool;
    public property FdDepthMap: TMFBool read FFdDepthMap;

    strict private FFdForeground: TSFNode;
    public property FdForeground: TSFNode read FFdForeground;

    strict private FFdZOffset: TMFInt32;
    public property FdZOffset: TMFInt32 read FFdZOffset;

    strict private FFdTargets: TMFNode;
    public property FdTargets: TMFNode read FFdTargets;

    strict private FFdDescription: TSFString;
    public property FdDescription: TSFString read FFdDescription;

    { Event out } { }
    strict private FEventViewing: TSFMatrix4fEvent;
    public property EventViewing: TSFMatrix4fEvent read FEventViewing;

    { Event out } { }
    strict private FEventProjection: TSFMatrix4fEvent;
    public property EventProjection: TSFMatrix4fEvent read FEventProjection;

    strict private FFdUpdate: TSFTextureUpdate;
    public property FdUpdate: TSFTextureUpdate read FFdUpdate;

    strict private FFdFrameBufferMode: TSFString;
    public property FdFrameBufferMode: TSFString read FFdFrameBufferMode;

    strict private FFdExcludeNodes: TMFNode;
    public property FdExcludeNodes: TMFNode read FFdExcludeNodes;

    strict private FFdTriggerName: TSFString;
    public property FdTriggerName: TSFString read FFdTriggerName;

    { Event in } { }
    strict private FEventTriggerSlot: TSFTimeEvent;
    public property EventTriggerSlot: TSFTimeEvent read FEventTriggerSlot;

    { Event out } { }
    strict private FEventImage_changed: TSFImageEvent;
    public property EventImage_changed: TSFImageEvent read FEventImage_changed;

    strict private FFdTextureProperties: TSFNode;
    public property FdTextureProperties: TSFNode read FFdTextureProperties;

    strict private FFdRepeatS: TSFBool;
    public property FdRepeatS: TSFBool read FFdRepeatS;

    strict private FFdRepeatT: TSFBool;
    public property FdRepeatT: TSFBool read FFdRepeatT;

    strict private FFdRepeatR: TSFBool;
    public property FdRepeatR: TSFBool read FFdRepeatR;

    { Event out } { }
    strict private FEventRendering: TSFBoolEvent;
    public property EventRendering: TSFBoolEvent read FEventRendering;

    {$I auto_generated_node_helpers/x3dnodes_renderedtexture.inc}
  end;

  TPlaneNode = class(TAbstractGeometryNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdSize: TSFVec2f;
    public property FdSize: TSFVec2f read FFdSize;

    strict private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;
  end;

  { Utility for setting and observing a boolean value in various ways. }
  TTogglerNode = class(TAbstractChildNode)
  strict private
    function IgnoreWhenDisabled(const ChangingField: TX3DField; const Event: TX3DEvent;
      const Value: TX3DField; const Time: TX3DTime): Boolean;
    procedure EventStatusReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    procedure EventNotStatusReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    procedure EventToggleReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    procedure EventSetReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
    procedure EventResetReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdStatus: TSFBool;
    public property FdStatus: TSFBool read FFdStatus;

    strict private FFdNotStatus: TSFBool;
    public property FdNotStatus: TSFBool read FFdNotStatus;

    { Event in } { }
    strict private FEventToggle: TXFAnyEvent;
    public property EventToggle: TXFAnyEvent read FEventToggle;

    { Event in } { }
    strict private FEventSet: TXFAnyEvent;
    public property EventSet: TXFAnyEvent read FEventSet;

    { Event in } { }
    strict private FEventReset: TXFAnyEvent;
    public property EventReset: TXFAnyEvent read FEventReset;

    { Event out } { }
    strict private FEventChanged: TSFBoolEvent;
    public property EventChanged: TSFBoolEvent read FEventChanged;

    { Event out } { }
    strict private FEventOn: TSFBoolEvent;
    public property EventOn: TSFBoolEvent read FEventOn;

    { Event out } { }
    strict private FEventOff: TSFBoolEvent;
    public property EventOff: TSFBoolEvent read FEventOff;

    strict private FFdEnabled: TSFBool;
    public property FdEnabled: TSFBool read FFdEnabled;

    {$I auto_generated_node_helpers/x3dnodes_toggler.inc}
  end;

  { Advanced material that can be heavily configured using textures
    (specular maps, normal maps and much more). }
  TCommonSurfaceShaderNode = class(TAbstractShaderNode)
  strict private
    type
      TCommonSurfaceShaderInfo = class(TPhongMaterialInfo)
      strict private
        FNode: TCommonSurfaceShaderNode;
      protected
        function GetDiffuseColor: TVector3; override;
        procedure SetDiffuseColor(const Value: TVector3); override;
        function GetSpecularColor: TVector3; override;
        procedure SetSpecularColor(const Value: TVector3); override;
      public
        constructor Create(ANode: TCommonSurfaceShaderNode);

        function AmbientColor: TVector3; override;
        function EmissiveColor: TVector3; override;
        function Shininess: Single; override;
        function ReflectionColor: TVector3; override;
        function TransmissionColor: TVector3; override;
        function Transparency: Single; override;
      end;
    var
      FMaterialInfo: TMaterialInfo;
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassX3DType: string; override;

    strict private FFdAlphaFactor: TSFFloat;
    public property FdAlphaFactor: TSFFloat read FFdAlphaFactor;

    strict private FFdAlphaTextureId: TSFInt32;
    public property FdAlphaTextureId: TSFInt32 read FFdAlphaTextureId;

    strict private FFdAlphaTextureCoordinatesId: TSFInt32;
    public property FdAlphaTextureCoordinatesId: TSFInt32 read FFdAlphaTextureCoordinatesId;

    strict private FFdAlphaTextureChannelMask: TSFString;
    public property FdAlphaTextureChannelMask: TSFString read FFdAlphaTextureChannelMask;

    strict private FFdAlphaTexture: TSFNode;
    public property FdAlphaTexture: TSFNode read FFdAlphaTexture;

    strict private FFdAmbientFactor: TSFVec3f;
    public property FdAmbientFactor: TSFVec3f read FFdAmbientFactor;

    strict private FFdAmbientTextureId: TSFInt32;
    public property FdAmbientTextureId: TSFInt32 read FFdAmbientTextureId;

    strict private FFdAmbientTextureCoordinatesId: TSFInt32;
    public property FdAmbientTextureCoordinatesId: TSFInt32 read FFdAmbientTextureCoordinatesId;

    strict private FFdAmbientTextureChannelMask: TSFString;
    public property FdAmbientTextureChannelMask: TSFString read FFdAmbientTextureChannelMask;

    strict private FFdAmbientTexture: TSFNode;
    public property FdAmbientTexture: TSFNode read FFdAmbientTexture;

    strict private FFdDiffuseFactor: TSFVec3f;
    public property FdDiffuseFactor: TSFVec3f read FFdDiffuseFactor;

    strict private FFdDiffuseTextureId: TSFInt32;
    public property FdDiffuseTextureId: TSFInt32 read FFdDiffuseTextureId;

    strict private FFdDiffuseTextureCoordinatesId: TSFInt32;
    public property FdDiffuseTextureCoordinatesId: TSFInt32 read FFdDiffuseTextureCoordinatesId;

    strict private FFdDiffuseTextureChannelMask: TSFString;
    public property FdDiffuseTextureChannelMask: TSFString read FFdDiffuseTextureChannelMask;

    strict private FFdDiffuseTexture: TSFNode;
    public property FdDiffuseTexture: TSFNode read FFdDiffuseTexture;

    strict private FFdDiffuseDisplacementTexture: TSFNode;
    public property FdDiffuseDisplacementTexture: TSFNode read FFdDiffuseDisplacementTexture;

    strict private FFdDisplacementAxis: TSFString;
    public property FdDisplacementAxis: TSFString read FFdDisplacementAxis;

    strict private FFdDisplacementFactor: TSFFloat;
    public property FdDisplacementFactor: TSFFloat read FFdDisplacementFactor;

    strict private FFdDisplacementTextureId: TSFInt32;
    public property FdDisplacementTextureId: TSFInt32 read FFdDisplacementTextureId;

    strict private FFdDisplacementTextureCoordinatesId: TSFInt32;
    public property FdDisplacementTextureCoordinatesId: TSFInt32 read FFdDisplacementTextureCoordinatesId;

    strict private FFdDisplacementTexture: TSFNode;
    public property FdDisplacementTexture: TSFNode read FFdDisplacementTexture;

    strict private FFdEmissiveFactor: TSFVec3f;
    public property FdEmissiveFactor: TSFVec3f read FFdEmissiveFactor;

    strict private FFdEmissiveTextureId: TSFInt32;
    public property FdEmissiveTextureId: TSFInt32 read FFdEmissiveTextureId;

    strict private FFdEmissiveTextureCoordinatesId: TSFInt32;
    public property FdEmissiveTextureCoordinatesId: TSFInt32 read FFdEmissiveTextureCoordinatesId;

    strict private FFdEmissiveTextureChannelMask: TSFString;
    public property FdEmissiveTextureChannelMask: TSFString read FFdEmissiveTextureChannelMask;

    strict private FFdEmissiveTexture: TSFNode;
    public property FdEmissiveTexture: TSFNode read FFdEmissiveTexture;

    strict private FFdEnvironmentFactor: TSFVec3f;
    public property FdEnvironmentFactor: TSFVec3f read FFdEnvironmentFactor;

    strict private FFdEnvironmentTextureId: TSFInt32;
    public property FdEnvironmentTextureId: TSFInt32 read FFdEnvironmentTextureId;

    strict private FFdEnvironmentTextureCoordinatesId: TSFInt32;
    public property FdEnvironmentTextureCoordinatesId: TSFInt32 read FFdEnvironmentTextureCoordinatesId;

    strict private FFdEnvironmentTextureChannelMask: TSFString;
    public property FdEnvironmentTextureChannelMask: TSFString read FFdEnvironmentTextureChannelMask;

    strict private FFdEnvironmentTexture: TSFNode;
    public property FdEnvironmentTexture: TSFNode read FFdEnvironmentTexture;

    strict private FFdMultiDiffuseAlphaTexture: TSFNode;
    public property FdMultiDiffuseAlphaTexture: TSFNode read FFdMultiDiffuseAlphaTexture;

    strict private FFdMultiEmmisiveAmbientIntensityTexture: TSFNode;
    public property FdMultiEmmisiveAmbientIntensityTexture: TSFNode read FFdMultiEmmisiveAmbientIntensityTexture;

    strict private FFdMultiSpecularShininessTexture: TSFNode;
    public property FdMultiSpecularShininessTexture: TSFNode read FFdMultiSpecularShininessTexture;

    strict private FFdMultiVisibilityTexture: TSFNode;
    public property FdMultiVisibilityTexture: TSFNode read FFdMultiVisibilityTexture;

    strict private FFdNormalFormat: TSFStringEnum;
    public property FdNormalFormat: TSFStringEnum read FFdNormalFormat;

    strict private FFdNormalSpace: TSFStringEnum;
    public property FdNormalSpace: TSFStringEnum read FFdNormalSpace;

    strict private FFdNormalTextureId: TSFInt32;
    public property FdNormalTextureId: TSFInt32 read FFdNormalTextureId;

    strict private FFdNormalTextureCoordinatesId: TSFInt32;
    public property FdNormalTextureCoordinatesId: TSFInt32 read FFdNormalTextureCoordinatesId;

    strict private FFdNormalTextureChannelMask: TSFString;
    public property FdNormalTextureChannelMask: TSFString read FFdNormalTextureChannelMask;

    strict private FFdNormalScale: TSFVec3f;
    public property FdNormalScale: TSFVec3f read FFdNormalScale;

    strict private FFdNormalBias: TSFVec3f;
    public property FdNormalBias: TSFVec3f read FFdNormalBias;

    strict private FFdNormalTexture: TSFNode;
    public property FdNormalTexture: TSFNode read FFdNormalTexture;

    strict private FFdNormalTextureParallaxHeight: TSFFloat;
    public property FdNormalTextureParallaxHeight: TSFFloat read FFdNormalTextureParallaxHeight;

    strict private FFdReflectionFactor: TSFVec3f;
    public property FdReflectionFactor: TSFVec3f read FFdReflectionFactor;

    strict private FFdReflectionTextureId: TSFInt32;
    public property FdReflectionTextureId: TSFInt32 read FFdReflectionTextureId;

    strict private FFdReflectionTextureCoordinatesId: TSFInt32;
    public property FdReflectionTextureCoordinatesId: TSFInt32 read FFdReflectionTextureCoordinatesId;

    strict private FFdReflectionTextureChannelMask: TSFString;
    public property FdReflectionTextureChannelMask: TSFString read FFdReflectionTextureChannelMask;

    strict private FFdReflectionTexture: TSFNode;
    public property FdReflectionTexture: TSFNode read FFdReflectionTexture;

    strict private FFdShininessFactor: TSFFloat;
    public property FdShininessFactor: TSFFloat read FFdShininessFactor;

    strict private FFdShininessTextureId: TSFInt32;
    public property FdShininessTextureId: TSFInt32 read FFdShininessTextureId;

    strict private FFdShininessTextureCoordinatesId: TSFInt32;
    public property FdShininessTextureCoordinatesId: TSFInt32 read FFdShininessTextureCoordinatesId;

    strict private FFdShininessTextureChannelMask: TSFString;
    public property FdShininessTextureChannelMask: TSFString read FFdShininessTextureChannelMask;

    strict private FFdShininessTexture: TSFNode;
    public property FdShininessTexture: TSFNode read FFdShininessTexture;

    strict private FFdSpecularFactor: TSFVec3f;
    public property FdSpecularFactor: TSFVec3f read FFdSpecularFactor;

    strict private FFdSpecularTextureId: TSFInt32;
    public property FdSpecularTextureId: TSFInt32 read FFdSpecularTextureId;

    strict private FFdSpecularTextureCoordinatesId: TSFInt32;
    public property FdSpecularTextureCoordinatesId: TSFInt32 read FFdSpecularTextureCoordinatesId;

    strict private FFdSpecularTextureChannelMask: TSFString;
    public property FdSpecularTextureChannelMask: TSFString read FFdSpecularTextureChannelMask;

    strict private FFdSpecularTexture: TSFNode;
    public property FdSpecularTexture: TSFNode read FFdSpecularTexture;

    strict private FFdTransmissionFactor: TSFVec3f;
    public property FdTransmissionFactor: TSFVec3f read FFdTransmissionFactor;

    strict private FFdTransmissionTextureId: TSFInt32;
    public property FdTransmissionTextureId: TSFInt32 read FFdTransmissionTextureId;

    strict private FFdTransmissionTextureCoordinatesId: TSFInt32;
    public property FdTransmissionTextureCoordinatesId: TSFInt32 read FFdTransmissionTextureCoordinatesId;

    strict private FFdTransmissionTextureChannelMask: TSFString;
    public property FdTransmissionTextureChannelMask: TSFString read FFdTransmissionTextureChannelMask;

    strict private FFdTransmissionTexture: TSFNode;
    public property FdTransmissionTexture: TSFNode read FFdTransmissionTexture;

    strict private FFdTangentTextureCoordinatesId: TSFInt32;
    public property FdTangentTextureCoordinatesId: TSFInt32 read FFdTangentTextureCoordinatesId;

    strict private FFdBinormalTextureCoordinatesId: TSFInt32;
    public property FdBinormalTextureCoordinatesId: TSFInt32 read FFdBinormalTextureCoordinatesId;

    strict private FFdInvertAlphaTexture: TSFBool;
    public property FdInvertAlphaTexture: TSFBool read FFdInvertAlphaTexture;

    strict private FFdRelativeIndexOfRefraction: TSFFloat;
    public property FdRelativeIndexOfRefraction: TSFFloat read FFdRelativeIndexOfRefraction;

    strict private FFdFresnelBlend: TSFFloat;
    public property FdFresnelBlend: TSFFloat read FFdFresnelBlend;

    strict private FFdTextureTransformEnabled: TMFBool;
    public property FdTextureTransformEnabled: TMFBool read FFdTextureTransformEnabled;

    { Shininess expressed as an exponent for shading equations.
      This is just @link(ShininessFactor) * 128. }
    function ShininessFactorExp: Single;

    { Transparency determined by the AlphaFactor.
      This is just 1 - @link(AlphaFactor). }
    function Transparency: Single;

    { Material information based on this node.
      It is automatically updated when properties of this material change.
      Do not free it yourself, it will be automatically freed when
      this node is freed. }
    function MaterialInfo: TMaterialInfo;

    {$I auto_generated_node_helpers/x3dnodes_commonsurfaceshader.inc}
  end;

  { Display a 2D image as a background. }
  TImageBackgroundNode = class(TAbstractBackgroundNode)
  protected
    function DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer; override;
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdColor: TSFColorRGBA;
    { @seealso Color }
    public property FdColor: TSFColorRGBA read FFdColor;

    strict private FFdTexture: TSFNode;
    { @seealso Texture }
    public property FdTexture: TSFNode read FFdTexture;

    strict private FFdTexCoords: TMFVec2f;
    { Texture coordinates of the full-screen quad.
      By default they use the whole texture area:

      @orderedList(
        @item Vector2(0, 0),
        @item Vector2(1, 0),
        @item Vector2(1, 1),
        @item Vector2(0, 1)
      )

      It is undefined what happens if there are not exactly 4 items. }
    public property FdTexCoords: TMFVec2f read FFdTexCoords;

    {$I auto_generated_node_helpers/x3dnodes_imagebackground.inc}
  end;
{$endif read_interface}

{$ifdef read_implementation}

{ TMatrixTransformNode.TMatrixTransformNodeTransformFunctionality --------------------------- }

constructor TMatrixTransformNode.TMatrixTransformNodeTransformFunctionality.
  Create(const AParent: TMatrixTransformNode);
begin
  inherited Create(AParent);
  FParent := AParent;
end;

procedure TMatrixTransformNode.TMatrixTransformNodeTransformFunctionality.
  ApplyTransform(var Transformation: TTransformation);
begin
  inherited;
  Transformation.MultiplyMatrix(Parent.Matrix);
end;

{ TMatrixTransformNode --------------------------------------------------------------- }

constructor TMatrixTransformNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FdMatrix.ChangeAlways := chTransform;

  FTransformFunctionality := TMatrixTransformNodeTransformFunctionality.Create(Self);
  AddFunctionality(FTransformFunctionality);
end;

class function TMatrixTransformNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TMatrixTransformNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FdChildren.Count - 1 do
  begin
    Result := Func(Self, FdChildren[I]);
    if Result <> nil then Exit;
  end;
end;

function TMatrixTransformNode.TransformationChange: TNodeTransformationChange;
begin
  Result := ntcTransform;
end;

constructor TTeapotNode.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  FdSize.ChangeAlways := chGeometry;
  FdSolid.ChangeAlways := chGeometry;
  FdManifold.ChangeAlways := chGeometry;
  FdTexCoord.ChangeAlways := chGeometry;
end;

function TTeapotNode.Proxy(var State: TX3DGraphTraverseState;
  const OverTriangulate: boolean): TAbstractGeometryNode;
const
  // Uncomment these if you need to temporarily compile without
  // auto_generated_teapot/teapot.inc.
  {
  TeapotCoord: array [0..0] of TVector3 = ((X: 0; Y: 0; Z: 0));
  TeapotCoordIndex: array [0..0] of LongInt = (0);
  TeapotManifoldCoord: array [0..0] of TVector3 = ((X: 0; Y: 0; Z: 0));
  TeapotManifoldCoordIndex: array [0..0] of LongInt = (0);
  }
  {$I auto_generated_teapot/teapot.inc}
var
  CoordNode: TCoordinateNode;
  IFS: TIndexedFaceSetNode absolute Result;
begin
  IFS := TIndexedFaceSetNode.Create(X3DName, BaseUrl);
  try
    CoordNode := TCoordinateNode.Create('', BaseUrl);
    IFS.FdCoord.Value := CoordNode;
    if FdManifold.Value then
      CoordNode.SetPoint(TeapotManifoldCoord)
    else
      CoordNode.SetPoint(TeapotCoord);

    { Scale Coords according to "size" field.
      Assumes that original coords are good for default size (3, 3, 3). }
    if not FdSize.EqualsDefaultValue then
    begin
      CoordNode.FdPoint.Items.MultiplyComponents(FdSize.Value * 1/3);
    end;

    if FdManifold.Value then
      IFS.FdCoordIndex.Items.AddRange(TeapotManifoldCoordIndex)
    else
      IFS.FdCoordIndex.Items.AddRange(TeapotCoordIndex);

    IFS.FdSolid.Value := FdSolid.Value;

    if FdTexCoord.CurrentChildAllowed then
      IFS.FdTexCoord.Value := FdTexCoord.Value;

    { Set CreaseAngle to anything larger than Pi, to make this completely
      smooth later using fast CreateSmoothNormalsCoordinateNode routine. }
    IFS.FdCreaseAngle.Value := 4;
  except FreeAndNil(Result); raise end;
end;

function TTeapotNode.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

function TTeapotNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := inherited;
  if Result <> nil then Exit;

  Result := FdtexCoord.Enumerate(Func);
  if Result <> nil then Exit;
end;

function TTeapotNode.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

function TTeapotNode.AutoGenerate3DTexCoords: boolean;
begin
  Result := (FdTexCoord.Value = nil) or not FdTexCoord.CurrentChildAllowed;
end;

constructor TLoggerNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FEventWrite.AddNotification({$ifdef FPC}@{$endif} EventWriteReceive);
end;

destructor TLoggerNode.Destroy;
begin
  FreeAndNil(LogFileStream);
  inherited;
end;

procedure TLoggerNode.EventWriteReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);

  function FieldValueToString(const Field: TX3DField): String;
  var
    Writer: TX3DWriterNames;
    StringStream: TStringStream;
  begin
    StringStream := TStringStream.Create('');
    try
      Writer := TX3DWriterNames.Create(StringStream, X3DVersion, xeClassic);
      try
        Field.FieldSaveToStream(Writer, true);
        // Remove trailing newline
        Result := TrimRight(StringStream.DataString);
      finally FreeAndNil(Writer) end;
    finally FreeAndNil(StringStream) end;
  end;

  procedure WritelnLogMessage;
  var
    S: string;
  begin
    if FdLevel.Value >= 2 then
    begin
      S := Format('Logger "%s": received field "%s" (%s). Time: %f.',
        [X3DName, Value.X3DName, Value.X3DType, Time.Seconds]);

      if (FdLevel.Value >= 3) and (Value.ParentNode <> nil) then
      begin
        S := S + Format(' Sending node: "%s" (%s).',
          [(Value.ParentNode as TX3DNode).X3DName,
           (Value.ParentNode as TX3DNode).X3DType]);
      end;

      S := S + ' Value: ' + FieldValueToString(Value);

      if LogFileStream <> nil then
        WriteLnStr(LogFileStream, S)
      else
        WriteLnLog(S);
    end;
  end;

begin
  if FdEnabled.Value and (Fdlevel.Value > 0) then
  begin
    if (LogFileStream = nil) and (FdLogFile.Value <> '') then
    begin
      LogFileStream := TFileStream.Create(
        FileNameAutoInc(ApplicationName + '_logger_' +
          DeleteURIExt(ExtractURIName(FdLogFile.Value)) + '_%d.log'), fmCreate);
    end;

    WritelnLogMessage;
  end;
end;

procedure TConverterNode.CreateNode;
begin
  inherited;

  FFdEnabled := TSFBool.Create(Self, true, 'enabled', true);
  AddField(FFdEnabled);

  FEventIn := TXFAnyEvent.Create(Self, 'in', true);
  AddEvent(FEventIn);
  FEventIn.AddNotification({$ifdef FPC}@{$endif} EventInReceive);

  FEventOut := TXFAnyEvent.Create(Self, 'out', true);
  AddEvent(FEventOut);
end;

procedure TConverterNode.EventInReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  if FdEnabled.Value then
    { TODO: we cannot convert here (as we don't know to what we should
      convert...) Conversion must be done along the route, when destination
      is known. }
    EventOut.Send(Value, Time);
end;

class function TConverterNode.ClassX3DType: string;
begin
  Result := 'Converter';
end;

procedure TRenderedTextureNode.CreateNode;
begin
  inherited;

  FFdViewpoint := TSFNode.Create(Self, true, 'viewpoint', [TAbstractViewpointNode, TViewpointMirrorNode]);
   FdViewpoint.ChangeAlways := chGeneratedTextureUpdateNeeded;
  AddField(FFdViewpoint);

  FFdBackground := TSFNode.Create(Self, true, 'background', [TAbstractBackgroundNode]);
   FdBackground.ChangeAlways := chGeneratedTextureUpdateNeeded;
  AddField(FFdBackground);

  FFdFog := TSFNode.Create(Self, true, 'fog', [TFogNode]);
   FdFog.ChangeAlways := chGeneratedTextureUpdateNeeded;
  AddField(FFdFog);

  FFdEnvironment := TSFNode.Create(Self, true, 'environment', [TX3DNode] { nothing sensible in core X3D for this });
   FdEnvironment.ChangeAlways := chGeneratedTextureUpdateNeeded;
  AddField(FFdEnvironment);

  FFdScene := TSFNode.Create(Self, true, 'scene', [TX3DNode]);
   FdScene.ChangeAlways := chGeneratedTextureUpdateNeeded;
  AddField(FFdScene);

  FFdDimensions := TMFInt32.Create(Self, true, 'dimensions',
    [ DefaultRenderedTextureWidth,
      DefaultRenderedTextureHeight, 4, 1, 1]);
   FdDimensions.ChangeAlways := chTextureRendererProperties;
  AddField(FFdDimensions);

  FFdDepthMap := TMFBool.Create(Self, true, 'depthMap', []);
   FdDepthMap.ChangeAlways := chTextureRendererProperties;
  AddField(FFdDepthMap);

  FFdForeground := TSFNode.Create(Self, true, 'foreground', [TX3DNode] { nothing sensible in core X3D for this });
   FdForeground.ChangeAlways := chGeneratedTextureUpdateNeeded;
  AddField(FFdForeground);

  FFdZOffset := TMFInt32.Create(Self, true, 'zOffset', []);
   FdZOffset.ChangeAlways := chTextureRendererProperties;
  AddField(FFdZOffset);

  FFdTargets := TMFNode.Create(Self, true, 'targets', [TAbstractTextureNode]);
   FdTargets.ChangeAlways := chTextureRendererProperties;
  AddField(FFdTargets);

  FFdDescription := TSFString.Create(Self, true, 'description', '');
  AddField(FFdDescription);

  FEventViewing := TSFMatrix4fEvent.Create(Self, 'viewing', false);
  AddEvent(FEventViewing);

  FEventProjection := TSFMatrix4fEvent.Create(Self, 'projection', false);
  AddEvent(FEventProjection);

  FFdUpdate := TSFTextureUpdate.Create(Self, true, 'update', upNone);
  AddField(FFdUpdate);

  FFdFrameBufferMode := TSFString.Create(Self, true, 'frameBufferMode', 'auto');
   FdFrameBufferMode.ChangeAlways := chTextureRendererProperties;
  AddField(FFdFrameBufferMode);

  FFdExcludeNodes := TMFNode.Create(Self, false, 'excludeNodes', [TX3DNode]);
   FdExcludeNodes.ChangeAlways := chGeneratedTextureUpdateNeeded;
  AddField(FFdExcludeNodes);

  FFdTriggerName := TSFString.Create(Self, false, 'triggerName', 'Render');
  AddField(FFdTriggerName);

  FEventTriggerSlot := TSFTimeEvent.Create(Self, 'triggerSlot', true);
  AddEvent(FEventTriggerSlot);

  FEventImage_changed := TSFImageEvent.Create(Self, 'image_changed', false);
  AddEvent(FEventImage_changed);

  FFdTextureProperties := TSFNode.Create(Self, false, 'textureProperties', [TTexturePropertiesNode]);
   FdTextureProperties.ChangeAlways := chTextureRendererProperties;
  AddField(FFdTextureProperties);

  FFdRepeatS := TSFBool.Create(Self, false, 'repeatS', true);
   FdRepeatS.ChangeAlways := chTextureRendererProperties;
  AddField(FFdRepeatS);

  FFdRepeatT := TSFBool.Create(Self, false, 'repeatT', true);
   FdRepeatT.ChangeAlways := chTextureRendererProperties;
  AddField(FFdRepeatT);

  FFdRepeatR := TSFBool.Create(Self, false, 'repeatR', true);
   FdRepeatR.ChangeAlways := chTextureRendererProperties;
  AddField(FFdRepeatR);

  FEventRendering := TSFBoolEvent.Create(Self, 'rendering', false);
  AddEvent(FEventRendering);

  FGeneratedTextureHandler := TGeneratedTextureHandler.Create;
  FGeneratedTextureHandler.FUpdate := FdUpdate;
end;

destructor TRenderedTextureNode.Destroy;
begin
  FreeAndNil(FGeneratedTextureHandler);
  inherited;
end;

class function TRenderedTextureNode.ClassX3DType: string;
begin
  Result := 'RenderedTexture';
end;

procedure TPlaneNode.CreateNode;
begin
  inherited;

  FFdSize := TSFVec2f.Create(Self, false, 'size', Vector2(2, 2));
   FdSize.ChangeAlways := chGeometry;
  AddField(FFdSize);
  { X3D specification comment: (0,Inf) }

  FFdSolid := TSFBool.Create(Self, false, 'solid', true);
   FdSolid.ChangeAlways := chGeometry;
  AddField(FFdSolid);
end;

class function TPlaneNode.ClassX3DType: string;
begin
  Result := 'Plane';
end;

function TPlaneNode.Proxy(var State: TX3DGraphTraverseState;
  const OverTriangulate: boolean): TAbstractGeometryNode;
begin
  Result := Rectangle2DProxy(Self, FdSize.Value, FdSolid.Value);
end;

function TPlaneNode.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

{ TTogglerNode --------------------------------------------------------------- }

constructor TTogglerNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FdStatus.OnInputIgnore := {$ifdef FPC}@{$endif} IgnoreWhenDisabled;
  FdNotStatus.OnInputIgnore := {$ifdef FPC}@{$endif} IgnoreWhenDisabled;
end;

function TTogglerNode.IgnoreWhenDisabled(const ChangingField: TX3DField; const Event: TX3DEvent;
  const Value: TX3DField; const Time: TX3DTime): Boolean;
begin
  { This makes status and notStatus input events ignored,
    and output events not generated when enabled = FALSE. }
  Result := not Enabled;
end;

procedure TTogglerNode.CreateNode;
begin
  inherited;

  FFdStatus := TSFBool.Create(Self, true, 'status', false);
   FdStatus.AddNotification({$ifdef FPC}@{$endif} EventStatusReceive);
  AddField(FFdStatus);

  FFdNotStatus := TSFBool.Create(Self, true, 'notStatus', true);
   FdNotstatus.AddNotification({$ifdef FPC}@{$endif} EventNotStatusReceive);
  AddField(FFdNotStatus);

  FEventToggle := TXFAnyEvent.Create(Self, 'toggle', true);
  FEventToggle.AddNotification({$ifdef FPC}@{$endif} EventToggleReceive);
  AddEvent(FEventToggle);

  FEventSet := TXFAnyEvent.Create(Self, 'set', true);
   EventSet.AddNotification({$ifdef FPC}@{$endif} EventSetReceive);
  AddEvent(FEventSet);

  FEventReset := TXFAnyEvent.Create(Self, 'reset', true);
   EventReset.AddNotification({$ifdef FPC}@{$endif} EventResetReceive);
  AddEvent(FEventReset);

  FEventChanged := TSFBoolEvent.Create(Self, 'changed', false);
  AddEvent(FEventChanged);

  FEventOn := TSFBoolEvent.Create(Self, 'on', false);
  AddEvent(FEventOn);

  FEventOff := TSFBoolEvent.Create(Self, 'off', false);
  AddEvent(FEventOff);

  FFdEnabled := TSFBool.Create(Self, true, 'enabled', true);
  AddField(FFdEnabled);
end;

class function TTogglerNode.ClassX3DType: string;
begin
  Result := 'Toggler';
end;

procedure TTogglerNode.EventStatusReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  { Do not send event to notStatus, if it already has correct value
    (so our own value didn't actually changed).
    This avoids calling status and notStatus events in a loop.
    By the way, also avoid sending excessive changed/on/off. }
  if FdnotStatus.Value <> not FdStatus.Value then
  begin
    FdnotStatus.Send(not FdStatus.Value, Value.InternalPartialSend);
    EventChanged.Send(true, Time, Value.InternalPartialSend);
    if FdStatus.Value then EventOn.Send(true, Time, Value.InternalPartialSend);
    if not FdStatus.Value then EventOff.Send(true, Time, Value.InternalPartialSend);
  end;
end;

procedure TTogglerNode.EventNotStatusReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  { Do not send event to status, if it already has correct value
    (so our own value didn't actually changed).
    This avoids calling status and notStatus events in a loop.
    By the way, also avoid sending excessive changed/on/off. }
  if FdnotStatus.Value <> not FdStatus.Value then
  begin
    FdStatus.Send(not FdnotStatus.Value, Value.InternalPartialSend);
    EventChanged.Send(true, Time, Value.InternalPartialSend);
    if FdStatus.Value then EventOn.Send(true, Time, Value.InternalPartialSend);
    if not FdStatus.Value then EventOff.Send(true, Time, Value.InternalPartialSend);
  end;
end;

procedure TTogglerNode.EventToggleReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  if FdEnabled.Value then
    FdStatus.Send(not FdStatus.Value, Value.InternalPartialSend);
end;

procedure TTogglerNode.EventSetReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  if FdEnabled.Value then
    FdStatus.Send(true, Value.InternalPartialSend);
end;

procedure TTogglerNode.EventResetReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  if FdEnabled.Value then
    FdStatus.Send(false, Value.InternalPartialSend);
end;

{ TCommonSurfaceShaderInfo --------------------------------------------------- }

constructor TCommonSurfaceShaderNode.TCommonSurfaceShaderInfo.Create(ANode: TCommonSurfaceShaderNode);
begin
  inherited Create(ANode);
  FNode := ANode;
end;

function TCommonSurfaceShaderNode.TCommonSurfaceShaderInfo.AmbientColor: TVector3;
begin
  Result := FNode.AmbientFactor;
end;

function TCommonSurfaceShaderNode.TCommonSurfaceShaderInfo.GetDiffuseColor: TVector3;
begin
  Result := FNode.DiffuseFactor;
end;

procedure TCommonSurfaceShaderNode.TCommonSurfaceShaderInfo.SetDiffuseColor(const Value: TVector3);
begin
  FNode.DiffuseFactor := Value;
end;

function TCommonSurfaceShaderNode.TCommonSurfaceShaderInfo.GetSpecularColor: TVector3;
begin
  Result := FNode.SpecularFactor;
end;

procedure TCommonSurfaceShaderNode.TCommonSurfaceShaderInfo.SetSpecularColor(const Value: TVector3);
begin
  FNode.SpecularFactor := Value;
end;

function TCommonSurfaceShaderNode.TCommonSurfaceShaderInfo.EmissiveColor: TVector3;
begin
  Result := FNode.EmissiveFactor;
end;

function TCommonSurfaceShaderNode.TCommonSurfaceShaderInfo.Shininess: Single;
begin
  Result := FNode.ShininessFactor;
end;

function TCommonSurfaceShaderNode.TCommonSurfaceShaderInfo.ReflectionColor: TVector3;
begin
  Result := FNode.ReflectionFactor;
end;

function TCommonSurfaceShaderNode.TCommonSurfaceShaderInfo.TransmissionColor: TVector3;
begin
  Result := FNode.TransmissionFactor;
end;

function TCommonSurfaceShaderNode.TCommonSurfaceShaderInfo.Transparency: Single;
begin
  Result := FNode.Transparency;
end;

{ TCommonSurfaceShaderNode ----------------------------------------------------- }

procedure TCommonSurfaceShaderNode.CreateNode;
begin
  inherited;

  FFdAlphaFactor := TSFFloat.Create(Self, true, 'alphaFactor', 1);
   FdAlphaFactor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdAlphaFactor);

  FFdAlphaTextureId := TSFInt32.Create(Self, true, 'alphaTextureId', -1);
   FdAlphaTextureId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdAlphaTextureId);

  FFdAlphaTextureCoordinatesId := TSFInt32.Create(Self, true, 'alphaTextureCoordinatesId', 0);
   FdAlphaTextureCoordinatesId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdAlphaTextureCoordinatesId);

  FFdAlphaTextureChannelMask := TSFString.Create(Self, true, 'alphaTextureChannelMask', 'a');
   FdAlphaTextureChannelMask.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdAlphaTextureChannelMask);

  FFdAlphaTexture := TSFNode.Create(Self, true, 'alphaTexture', [TAbstractTextureNode]);
   FdAlphaTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdAlphaTexture);

  FFdAmbientFactor := TSFVec3f.Create(Self, true, 'ambientFactor', Vector3(0.2, 0.2, 0.2));
   FdAmbientFactor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdAmbientFactor);

  FFdAmbientTextureId := TSFInt32.Create(Self, true, 'ambientTextureId', -1);
   FdAmbientTextureId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdAmbientTextureId);

  FFdAmbientTextureCoordinatesId := TSFInt32.Create(Self, true, 'ambientTextureCoordinatesId', 0);
   FdAmbientTextureCoordinatesId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdAmbientTextureCoordinatesId);

  FFdAmbientTextureChannelMask := TSFString.Create(Self, true, 'ambientTextureChannelMask', 'rgb');
   FdAmbientTextureChannelMask.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdAmbientTextureChannelMask);

  FFdAmbientTexture := TSFNode.Create(Self, true, 'ambientTexture', [TAbstractTextureNode]);
   FdAmbientTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdAmbientTexture);

  FFdDiffuseFactor := TSFVec3f.Create(Self, true, 'diffuseFactor', Vector3(0.8, 0.8, 0.8));
   FdDiffuseFactor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdDiffuseFactor);

  FFdDiffuseTextureId := TSFInt32.Create(Self, true, 'diffuseTextureId', -1);
   FdDiffuseTextureId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdDiffuseTextureId);

  FFdDiffuseTextureCoordinatesId := TSFInt32.Create(Self, true, 'diffuseTextureCoordinatesId', 0);
   FdDiffuseTextureCoordinatesId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdDiffuseTextureCoordinatesId);

  FFdDiffuseTextureChannelMask := TSFString.Create(Self, true, 'diffuseTextureChannelMask', 'rgb');
   FdDiffuseTextureChannelMask.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdDiffuseTextureChannelMask);

  FFdDiffuseTexture := TSFNode.Create(Self, true, 'diffuseTexture', [TAbstractTextureNode]);
   FdDiffuseTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdDiffuseTexture);

  FFdDiffuseDisplacementTexture := TSFNode.Create(Self, true, 'diffuseDisplacementTexture', [TAbstractTextureNode]);
   FdDiffuseDisplacementTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdDiffuseDisplacementTexture);

  FFdDisplacementAxis := TSFString.Create(Self, true, 'displacementAxis', 'y');
   FdDisplacementAxis.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdDisplacementAxis);

  FFdDisplacementFactor := TSFFloat.Create(Self, true, 'displacementFactor', 255.0);
   FdDisplacementFactor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdDisplacementFactor);

  FFdDisplacementTextureId := TSFInt32.Create(Self, true, 'displacementTextureId', -1);
   FdDisplacementTextureId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdDisplacementTextureId);

  FFdDisplacementTextureCoordinatesId := TSFInt32.Create(Self, true, 'displacementTextureCoordinatesId', 0);
   FdDisplacementTextureCoordinatesId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdDisplacementTextureCoordinatesId);

  FFdDisplacementTexture := TSFNode.Create(Self, true, 'displacementTexture', [TAbstractTextureNode]);
   FdDisplacementTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdDisplacementTexture);

  FFdEmissiveFactor := TSFVec3f.Create(Self, true, 'emissiveFactor', Vector3(0, 0, 0));
   FdEmissiveFactor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdEmissiveFactor);

  FFdEmissiveTextureId := TSFInt32.Create(Self, true, 'emissiveTextureId', -1);
   FdEmissiveTextureId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdEmissiveTextureId);

  FFdEmissiveTextureCoordinatesId := TSFInt32.Create(Self, true, 'emissiveTextureCoordinatesId', 0);
   FdEmissiveTextureCoordinatesId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdEmissiveTextureCoordinatesId);

  FFdEmissiveTextureChannelMask := TSFString.Create(Self, true, 'emissiveTextureChannelMask', 'rgb');
   FdEmissiveTextureChannelMask.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdEmissiveTextureChannelMask);

  FFdEmissiveTexture := TSFNode.Create(Self, true, 'emissiveTexture', [TAbstractTextureNode]);
   FdEmissiveTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdEmissiveTexture);

  FFdEnvironmentFactor := TSFVec3f.Create(Self, true, 'environmentFactor', Vector3(1, 1, 1));
   FdEnvironmentFactor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdEnvironmentFactor);

  FFdEnvironmentTextureId := TSFInt32.Create(Self, true, 'environmentTextureId', -1);
   FdEnvironmentTextureId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdEnvironmentTextureId);

  FFdEnvironmentTextureCoordinatesId := TSFInt32.Create(Self, true, 'environmentTextureCoordinatesId', 0);
   FdEnvironmentTextureCoordinatesId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdEnvironmentTextureCoordinatesId);

  FFdEnvironmentTextureChannelMask := TSFString.Create(Self, true, 'environmentTextureChannelMask', 'rgb');
   FdEnvironmentTextureChannelMask.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdEnvironmentTextureChannelMask);

  FFdEnvironmentTexture := TSFNode.Create(Self, true, 'environmentTexture', [TAbstractEnvironmentTextureNode]);
   FdEnvironmentTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdEnvironmentTexture);

  FFdMultiDiffuseAlphaTexture := TSFNode.Create(Self, true, 'multiDiffuseAlphaTexture', [TAbstractTextureNode]);
   FdMultiDiffuseAlphaTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdMultiDiffuseAlphaTexture);

  FFdMultiEmmisiveAmbientIntensityTexture := TSFNode.Create(Self, true, 'multiEmmisiveAmbientIntensityTexture', [TAbstractTextureNode]);
   FdMultiEmmisiveAmbientIntensityTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdMultiEmmisiveAmbientIntensityTexture);

  FFdMultiSpecularShininessTexture := TSFNode.Create(Self, true, 'multiSpecularShininessTexture', [TAbstractTextureNode]);
   FdMultiSpecularShininessTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdMultiSpecularShininessTexture);

  FFdMultiVisibilityTexture := TSFNode.Create(Self, true, 'multiVisibilityTexture', [TAbstractTextureNode]);
   FdMultiVisibilityTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdMultiVisibilityTexture);

  FFdNormalFormat := TSFStringEnum.Create(Self, true, 'normalFormat', ['UNORM'], 0);
   FdNormalFormat.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdNormalFormat);
  { X3D specification comment: ["UNORM"] }

  FFdNormalSpace := TSFStringEnum.Create(Self, true, 'normalSpace', ['TANGENT'], 0);
   FdNormalSpace.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdNormalSpace);
  { X3D specification comment: ["TANGENT"] }

  FFdNormalTextureId := TSFInt32.Create(Self, true, 'normalTextureId', -1);
   FdNormalTextureId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdNormalTextureId);

  FFdNormalTextureCoordinatesId := TSFInt32.Create(Self, true, 'normalTextureCoordinatesId', 0);
   FdNormalTextureCoordinatesId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdNormalTextureCoordinatesId);

  FFdNormalTextureChannelMask := TSFString.Create(Self, true, 'normalTextureChannelMask', 'rgb');
   FdNormalTextureChannelMask.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdNormalTextureChannelMask);

  FFdNormalScale := TSFVec3f.Create(Self, false, 'normalScale', Vector3(2, 2, 2));
   FdNormalScale.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdNormalScale);

  FFdNormalBias := TSFVec3f.Create(Self, false, 'normalBias', Vector3(-1, -1, -1));
   FdNormalBias.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdNormalBias);

  FFdNormalTexture := TSFNode.Create(Self, true, 'normalTexture', [TAbstractTextureNode]);
   FdNormalTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdNormalTexture);

  FFdNormalTextureParallaxHeight := TSFFloat.Create(Self, true, 'normalTextureParallaxHeight', 0);
   FdNormalTextureParallaxHeight.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdNormalTextureParallaxHeight);

  FFdReflectionFactor := TSFVec3f.Create(Self, true, 'reflectionFactor', Vector3(0, 0, 0));
   FdReflectionFactor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdReflectionFactor);

  FFdReflectionTextureId := TSFInt32.Create(Self, true, 'reflectionTextureId', -1);
   FdReflectionTextureId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdReflectionTextureId);

  FFdReflectionTextureCoordinatesId := TSFInt32.Create(Self, true, 'reflectionTextureCoordinatesId', 0);
   FdReflectionTextureCoordinatesId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdReflectionTextureCoordinatesId);

  FFdReflectionTextureChannelMask := TSFString.Create(Self, true, 'reflectionTextureChannelMask', 'rgb');
   FdReflectionTextureChannelMask.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdReflectionTextureChannelMask);

  FFdReflectionTexture := TSFNode.Create(Self, true, 'reflectionTexture', [TAbstractTextureNode]);
   FdReflectionTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdReflectionTexture);

  FFdShininessFactor := TSFFloat.Create(Self, true, 'shininessFactor', 0.2);
   FdShininessFactor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdShininessFactor);

  FFdShininessTextureId := TSFInt32.Create(Self, true, 'shininessTextureId', -1);
   FdShininessTextureId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdShininessTextureId);

  FFdShininessTextureCoordinatesId := TSFInt32.Create(Self, true, 'shininessTextureCoordinatesId', 0);
   FdShininessTextureCoordinatesId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdShininessTextureCoordinatesId);

  FFdShininessTextureChannelMask := TSFString.Create(Self, true, 'shininessTextureChannelMask', 'a');
   FdShininessTextureChannelMask.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdShininessTextureChannelMask);

  FFdShininessTexture := TSFNode.Create(Self, true, 'shininessTexture', [TAbstractTextureNode]);
   FdShininessTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdShininessTexture);

  FFdSpecularFactor := TSFVec3f.Create(Self, true, 'specularFactor', Vector3(0, 0, 0));
   FdSpecularFactor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdSpecularFactor);

  FFdSpecularTextureId := TSFInt32.Create(Self, true, 'specularTextureId', -1);
   FdSpecularTextureId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdSpecularTextureId);

  FFdSpecularTextureCoordinatesId := TSFInt32.Create(Self, true, 'specularTextureCoordinatesId', 0);
   FdSpecularTextureCoordinatesId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdSpecularTextureCoordinatesId);

  FFdSpecularTextureChannelMask := TSFString.Create(Self, true, 'specularTextureChannelMask', 'rgb');
   FdSpecularTextureChannelMask.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdSpecularTextureChannelMask);

  FFdSpecularTexture := TSFNode.Create(Self, true, 'specularTexture', [TAbstractTextureNode]);
   FdSpecularTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdSpecularTexture);

  FFdTransmissionFactor := TSFVec3f.Create(Self, true, 'transmissionFactor', Vector3(0, 0, 0));
   FdTransmissionFactor.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdTransmissionFactor);

  FFdTransmissionTextureId := TSFInt32.Create(Self, true, 'transmissionTextureId', -1);
   FdTransmissionTextureId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdTransmissionTextureId);

  FFdTransmissionTextureCoordinatesId := TSFInt32.Create(Self, true, 'transmissionTextureCoordinatesId', 0);
   FdTransmissionTextureCoordinatesId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdTransmissionTextureCoordinatesId);

  FFdTransmissionTextureChannelMask := TSFString.Create(Self, true, 'transmissionTextureChannelMask', 'rgb');
   FdTransmissionTextureChannelMask.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdTransmissionTextureChannelMask);

  FFdTransmissionTexture := TSFNode.Create(Self, true, 'transmissionTexture', [TAbstractTextureNode]);
   FdTransmissionTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdTransmissionTexture);

  FFdTangentTextureCoordinatesId := TSFInt32.Create(Self, true, 'tangentTextureCoordinatesId', -1);
   FdTangentTextureCoordinatesId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdTangentTextureCoordinatesId);

  FFdBinormalTextureCoordinatesId := TSFInt32.Create(Self, true, 'binormalTextureCoordinatesId', -1);
   FdBinormalTextureCoordinatesId.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdBinormalTextureCoordinatesId);

  FFdInvertAlphaTexture := TSFBool.Create(Self, true, 'invertAlphaTexture', false);
   FdInvertAlphaTexture.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdInvertAlphaTexture);

  FFdRelativeIndexOfRefraction := TSFFloat.Create(Self, true, 'relativeIndexOfRefraction', 1);
   FdRelativeIndexOfRefraction.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdRelativeIndexOfRefraction);

  FFdFresnelBlend := TSFFloat.Create(Self, true, 'fresnelBlend', 0);
   FdFresnelBlend.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdFresnelBlend);

  FFdTextureTransformEnabled := TMFBool.Create(Self, false, 'textureTransformEnabled', [false, false, false, false, false, false, false, false]);
   FdTextureTransformEnabled.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdTextureTransformEnabled);

  DefaultContainerField := 'shaders';
end;

destructor TCommonSurfaceShaderNode.Destroy;
begin
  FreeAndNil(FMaterialInfo);
  inherited;
end;

class function TCommonSurfaceShaderNode.ClassX3DType: string;
begin
  Result := 'CommonSurfaceShader';
end;

function TCommonSurfaceShaderNode.ShininessFactorExp: Single;
begin
  Result := Clamped(FdShininessFactor.Value * 128.0, 0.0, 128.0);
end;

function TCommonSurfaceShaderNode.Transparency: Single;
begin
  Result := 1 - FdAlphaFactor.Value;
end;

function TCommonSurfaceShaderNode.MaterialInfo: TMaterialInfo;
begin
  if FMaterialInfo = nil then
    FMaterialInfo := TCommonSurfaceShaderInfo.Create(Self);
  Result := FMaterialInfo;
end;

{ TImageBackgroundNode ------------------------------------------------------- }

procedure TImageBackgroundNode.CreateNode;
begin
  inherited;

  FFdColor := TSFColorRGBA.Create(Self, true, 'color', White);
  AddField(FFdColor);

  FFdTexture := TSFNode.Create(Self, true, 'texture', [TAbstractTextureNode]);
  AddField(FFdTexture);

  FFdTexCoords := TMFVec2f.Create(Self, true, 'texCoords', [
    Vector2(0, 0),
    Vector2(1, 0),
    Vector2(1, 1),
    Vector2(0, 1)
  ]);
  AddField(FFdTexCoords);
end;

class function TImageBackgroundNode.ClassX3DType: string;
begin
  Result := 'ImageBackground';
end;

function TImageBackgroundNode.DirectEnumerateActive(Func: TEnumerateChildrenFunction): Pointer;
begin
  Result := inherited;
  if Result <> nil then Exit;

  Result := FdTexture.Enumerate(Func);
  if Result <> nil then Exit;
end;

{ registration ----------------------------------------------------------------- }

procedure RegisterInstantRealityNodes;
begin
  NodesManager.RegisterNodeClasses([
    TMatrixTransformNode,
    TTeapotNode,
    TLoggerNode,
    TConverterNode,
    TRenderedTextureNode,
    TPlaneNode,
    TTogglerNode,
    TCommonSurfaceShaderNode,
    TImageBackgroundNode
  ]);
end;
{$endif read_implementation}
