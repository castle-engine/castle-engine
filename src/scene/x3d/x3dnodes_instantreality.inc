{
  Copyright 2008-2023 Michalis Kamburelis.

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
  TMatrixTransformNode = class(TAbstractGroupingNode)
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
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;
    function TexCoordField: TSFNode; override;
    function AutoGenerate3DTexCoords: boolean; override;
    function SolidField: TSFBool; override;

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
    constructor Create(const AX3DName, ABaseUrl: String); override;
  {$I auto_generated_node_helpers/x3dnodes_converter.inc}
  end;

  { Texture with contents created by rendering the scene from a specific
    viewpoint. }
  TRenderedTextureNode = class(TAbstractSingleTextureNode)
  strict private
    type
      TRenderedTextureGenTex = class(TGeneratedTextureFunctionality)
      strict private
        FParent: TRenderedTextureNode;
      protected
        function GetUpdate: TTextureUpdate; override;
        procedure SetUpdate(const Value: TTextureUpdate); override;
      public
        constructor Create(const AParent: TRenderedTextureNode);
      end;
    var
      FGenTexFunctionality: TRenderedTextureGenTex;
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
  {$I auto_generated_node_helpers/x3dnodes_renderedtexture.inc}
  end;

  { Alternative (deprecated) version of TRectangle2DNode,
    compatible with InstantReality X3D browser.
    @deprecated }
  TPlaneNode = class(TAbstractGeometryNode)
  public
    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;
    function SolidField: TSFBool; override;

  {$I auto_generated_node_helpers/x3dnodes_plane.inc}
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
    destructor Destroy; override;

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

function TTeapotNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
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

function TTeapotNode.SolidField: TSFBool;
begin
  Result := FdSolid;
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

{ TConverterNode ------------------------------------------------------------- }

constructor TConverterNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FEventIn.AddNotification({$ifdef FPC}@{$endif} EventInReceive);
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

{ TRenderedTextureNode.TRenderedTextureGenTex -------------------------------- }

constructor TRenderedTextureNode.TRenderedTextureGenTex.Create(const AParent: TRenderedTextureNode);
begin
  inherited Create(AParent);
  FParent := AParent;
end;

function TRenderedTextureNode.TRenderedTextureGenTex.GetUpdate: TTextureUpdate;
begin
  Result := FParent.Update;
end;

procedure TRenderedTextureNode.TRenderedTextureGenTex.SetUpdate(const Value: TTextureUpdate);
begin
  FParent.Update := Value;
end;

{ TRenderedTextureNode ------------------------------------------------------- }

constructor TRenderedTextureNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FGenTexFunctionality := TRenderedTextureGenTex.Create(Self);
  AddFunctionality(FGenTexFunctionality);
end;

{ TPlaneNode ----------------------------------------------------------------- }

function TPlaneNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
begin
  Result := Rectangle2DProxy(Self, FdSize.Value, FdSolid.Value);
end;

function TPlaneNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

{ TTogglerNode --------------------------------------------------------------- }

constructor TTogglerNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;

  FdStatus.AddNotification({$ifdef FPC}@{$endif} EventStatusReceive);
  FdNotstatus.AddNotification({$ifdef FPC}@{$endif} EventNotStatusReceive);
  FEventToggle.AddNotification({$ifdef FPC}@{$endif} EventToggleReceive);
  EventSet.AddNotification({$ifdef FPC}@{$endif} EventSetReceive);
  EventReset.AddNotification({$ifdef FPC}@{$endif} EventResetReceive);

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

destructor TCommonSurfaceShaderNode.Destroy;
begin
  FreeAndNil(FMaterialInfo);
  inherited;
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
