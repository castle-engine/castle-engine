{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Interpolation mode, for @link(TAbstractInterpolatorNode.Interpolation). }
  TInterpolation = (
    { The value changes linearly from previous to next.
      Some interpolators have a specific algorithm for "linear change",
      for example colors are interpolated in HSV (LerpRgbInHsv),
      rotations are interpolated using Slerp. }
    inLinear,

    { The value changes immediately from previous to next,
      at the time of next key.
      Before the time of 1st key, the value is equal to the 1st value.

      E.g. key = [ 0.1, 0.2 ], keyValue = [ 1 2 ] =>
      value is 1 when time is [0, 0.2),
      value is 2 when time is [0.2, ...].

      This is consistent with glTF "step" interpolation,
      see https://www.khronos.org/registry/glTF/specs/2.0/glTF-2.0.html#appendix-c-interpolation . }
    inStep
  );

  { Basis for all nodes interpolating (animating) values. }
  TAbstractInterpolatorNode = class(TAbstractChildNode)
  strict protected
    function KeyRange(const Fraction: Single; out T: Single): Integer;

  {$I auto_generated_node_helpers/x3dnodes_x3dinterpolatornode.inc}
  end;

  { Common base class for all implemented interpolator nodes. }
  {$ifdef FPC}generic{$endif}
  TAbstractCommonInterpolatorNode<
    TOutputField: TX3DField> = class(TAbstractInterpolatorNode)
  strict private
    FEventValue_changed: TX3DEvent;
    FTempFields: array of TOutputField;
    FTempFieldsUsed: TListSize;
    { Check is Index < KeyValueCount.
      Makes warning when returning false. }
    function CheckKeyValue(const Index: TListSize): Boolean;
    procedure EventSet_FractionReceive(const Event: TX3DEvent; const Value: TX3DField;
      const Time: TX3DTime);
  strict protected
    { Values count determines allowed indexes for InterpolatorLerp, InterpolatorSet. }
    function KeyValueCount: TListSize; virtual; abstract;
    { Set Target.Value to Lerp(A, KeyValue[Index1], KeyValue[Index2]).
      Index1 and Index2 are for sure valid (between 0 and KeyValueCount-1). }
    procedure InterpolatorLerp(const Target: TOutputField;
      const A: Single; const Index1, Index2: Integer); virtual; abstract;
    { Set Target.Value to KeyValue[Index].
      Index is for sure valid (between 0 and KeyValueCount-1). }
    procedure InterpolatorSet(const Target: TOutputField; const Index: Integer); virtual; abstract;
    { Create TOutputField instance, to send it by EventValue_Changed. }
    function CreateOutputField: TOutputField; virtual; abstract;
  public
    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    destructor Destroy; override;
  end;

  { Interpolate (animate) a single color. }
  TColorInterpolatorNode = class({$ifdef FPC}specialize{$endif} TAbstractCommonInterpolatorNode<
    TSFVec3f>)
  strict protected
    function KeyValueCount: TListSize; override;
    procedure InterpolatorLerp(const Target: TSFVec3f; const A: Single;
      const Index1, Index2: Integer); override;
    procedure InterpolatorSet(const Target: TSFVec3f; const Index: Integer); override;
    function CreateOutputField: TSFVec3f; override;

  {$I auto_generated_node_helpers/x3dnodes_colorinterpolator.inc}
  end;

  { Interpolate (animate) a set of colors,
    for example to animate a set of @link(TBackgroundNode)
    or @link(TColorNode) colors. }
  TColorSetInterpolatorNode = class({$ifdef FPC}specialize{$endif} TAbstractCommonInterpolatorNode<
    TMFColor>)
  strict private
    function FrameValues: TListSize;
  strict protected
    function KeyValueCount: TListSize; override;
    procedure InterpolatorLerp(const Target: TMFColor; const A: Single;
      const Index1, Index2: Integer); override;
    procedure InterpolatorSet(const Target: TMFColor; const Index: Integer); override;
    function CreateOutputField: TMFColor; override;

  {$I auto_generated_node_helpers/x3dnodes_colorsetinterpolator.inc}
  end;

  { Interpolate (animate) a set of 3D positions,
    for example to animate @link(TCoordinateNode coordinates) of a mesh. }
  TCoordinateInterpolatorNode = class({$ifdef FPC}specialize{$endif} TAbstractCommonInterpolatorNode<
    TMFVec3f>)
  strict private
    function FrameValues: TListSize;
  strict protected
    function KeyValueCount: TListSize; override;
    procedure InterpolatorLerp(const Target: TMFVec3f; const A: Single;
      const Index1, Index2: Integer); override;
    procedure InterpolatorSet(const Target: TMFVec3f; const Index: Integer); override;
    function CreateOutputField: TMFVec3f; override;

  {$I auto_generated_node_helpers/x3dnodes_coordinateinterpolator.inc}
  end;

  { Interpolate (animate) a set of 2D positions,
    for example to animate 2D texture coordinates. }
  TCoordinateInterpolator2DNode = class({$ifdef FPC}specialize{$endif} TAbstractCommonInterpolatorNode<
    TMFVec2f>)
  strict private
    function FrameValues: TListSize;
  strict protected
    function KeyValueCount: TListSize; override;
    procedure InterpolatorLerp(const Target: TMFVec2f; const A: Single;
      const Index1, Index2: Integer); override;
    procedure InterpolatorSet(const Target: TMFVec2f; const Index: Integer); override;
    function CreateOutputField: TMFVec2f; override;

  {$I auto_generated_node_helpers/x3dnodes_coordinateinterpolator2d.inc}
  end;

  { Support controlled gradual transitions by modifying
    TimeSensor node fractions.
    @bold(Not implemented yet.) }
  TEaseInEaseOutNode = class(TAbstractChildNode)
  {$I auto_generated_node_helpers/x3dnodes_easeineaseout.inc}
  end;

  { Interpolate (animate) a set of 3D directions, for example normal vectors. }
  TNormalInterpolatorNode = class({$ifdef FPC}specialize{$endif} TAbstractCommonInterpolatorNode<
    TMFVec3f>)
  strict private
    function FrameValues: TListSize;
  strict protected
    function KeyValueCount: TListSize; override;
    procedure InterpolatorLerp(const Target: TMFVec3f; const A: Single;
      const Index1, Index2: Integer); override;
    procedure InterpolatorSet(const Target: TMFVec3f; const Index: Integer); override;
    function CreateOutputField: TMFVec3f; override;

  {$I auto_generated_node_helpers/x3dnodes_normalinterpolator.inc}
  end;

  { Interpolate (animate) an orientation,
    for example to animate @link(TTransformNode.Rotation). }
  TOrientationInterpolatorNode = class({$ifdef FPC}specialize{$endif} TAbstractCommonInterpolatorNode<
    TSFRotation>)
  strict protected
    function KeyValueCount: TListSize; override;
    procedure InterpolatorLerp(const Target: TSFRotation; const A: Single;
      const Index1, Index2: Integer); override;
    procedure InterpolatorSet(const Target: TSFRotation; const Index: Integer); override;
    function CreateOutputField: TSFRotation; override;
  public
    { Return rotation (as axis+angle) at given time (key).
      Makes a warning and returns zero if FdKey is empty. }
    function Interpolate(const TimeKey: Single): TVector4;

  {$I auto_generated_node_helpers/x3dnodes_orientationinterpolator.inc}
  end;

  { Interpolate (animate) a 3D position,
    for example to animate @link(TTransformNode.Translation). }
  TPositionInterpolatorNode = class({$ifdef FPC}specialize{$endif} TAbstractCommonInterpolatorNode<
    TSFVec3f>)
  strict protected
    function KeyValueCount: TListSize; override;
    procedure InterpolatorLerp(const Target: TSFVec3f; const A: Single;
      const Index1, Index2: Integer); override;
    procedure InterpolatorSet(const Target: TSFVec3f; const Index: Integer); override;
    function CreateOutputField: TSFVec3f; override;
  public
    { Return position at given time (key).
      Makes a warning and returns zero if FdKey is empty. }
    function Interpolate(const TimeKey: Single): TVector3;

  {$I auto_generated_node_helpers/x3dnodes_positioninterpolator.inc}
  end;

  { Interpolate (animate) a 2D position. }
  TPositionInterpolator2DNode = class({$ifdef FPC}specialize{$endif} TAbstractCommonInterpolatorNode<
    TSFVec2f>)
  strict protected
    function KeyValueCount: TListSize; override;
    procedure InterpolatorLerp(const Target: TSFVec2f; const A: Single;
      const Index1, Index2: Integer); override;
    procedure InterpolatorSet(const Target: TSFVec2f; const Index: Integer); override;
    function CreateOutputField: TSFVec2f; override;

  {$I auto_generated_node_helpers/x3dnodes_positioninterpolator2d.inc}
  end;

  { Interpolate (animate) a single floating-point value,
    for example to animate @link(TMaterialNode.Transparency). }
  TScalarInterpolatorNode = class({$ifdef FPC}specialize{$endif} TAbstractCommonInterpolatorNode<
    TSFFloat>)
  strict protected
    function KeyValueCount: TListSize; override;
    procedure InterpolatorLerp(const Target: TSFFloat; const A: Single;
      const Index1, Index2: Integer); override;
    procedure InterpolatorSet(const Target: TSFFloat; const Index: Integer); override;
    function CreateOutputField: TSFFloat; override;

  {$I auto_generated_node_helpers/x3dnodes_scalarinterpolator.inc}
  end;

  { Non-linearly interpolate (animate) a 3D position.
    @bold(Not implemented yet.) }
  TSplinePositionInterpolatorNode = class(TAbstractInterpolatorNode)
  {$I auto_generated_node_helpers/x3dnodes_splinepositioninterpolator.inc}
  end;

  { Non-linearly interpolate (animate) a 2D position.
    @bold(Not implemented yet.) }
  TSplinePositionInterpolator2DNode = class(TAbstractInterpolatorNode)
  {$I auto_generated_node_helpers/x3dnodes_splinepositioninterpolator2d.inc}
  end;

  { Non-linearly interpolate (animate) a single floating-point value.
    @bold(Not implemented yet.) }
  TSplineScalarInterpolatorNode = class(TAbstractInterpolatorNode)
  {$I auto_generated_node_helpers/x3dnodes_splinescalarinterpolator.inc}
  end;

  { Non-linearly interpolate (animate) a rotation.
    @bold(Not implemented yet.) }
  TSquadOrientationInterpolatorNode = class(TAbstractInterpolatorNode)
  {$I auto_generated_node_helpers/x3dnodes_squadorientationinterpolator.inc}
  end;

  { Interpolate (animate) a set of floats, for example to animate
    @link(TElevationGridNode) heights. }
  TVectorInterpolatorNode = class({$ifdef FPC}specialize{$endif} TAbstractCommonInterpolatorNode<
    TMFFloat>)
  strict private
    function FrameValues: TListSize;
  strict protected
    function KeyValueCount: TListSize; override;
    procedure InterpolatorLerp(const Target: TMFFloat; const A: Single;
      const Index1, Index2: Integer); override;
    procedure InterpolatorSet(const Target: TMFFloat; const Index: Integer); override;
    function CreateOutputField: TMFFloat; override;

  {$I auto_generated_node_helpers/x3dnodes_vectorinterpolator.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

const
  InterpolationNames: array [TInterpolation] of String = (
    'LINEAR',
    'STEP'
  );

{ TAbstractInterpolatorNode -------------------------------------------------- }

function TAbstractInterpolatorNode.KeyRange(
  const Fraction: Single; out T: Single): Integer;
begin
  Result := X3DNodes.KeyRange(FdKey.Items, Fraction, T);
end;

{ TAbstractCommonInterpolatorNode -------------------------------------------- }

constructor TAbstractCommonInterpolatorNode
  {$ifndef FPC}<TOutputField>{$endif} .
  Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  FEventValue_changed := AnyEvent('value_changed', true);
  EventSet_Fraction.AddNotification(
    {$ifdef FPC}@{$endif} EventSet_FractionReceive);
end;

destructor TAbstractCommonInterpolatorNode
  {$ifndef FPC}<TOutputField>{$endif} .
  Destroy;
var
  I: TListSize;
begin
  for I := 0 to High(FTempFields) do
    FreeAndNil(FTempFields[I]);
  inherited;
end;

function TAbstractCommonInterpolatorNode
  {$ifndef FPC}<TOutputField>{$endif} .
  CheckKeyValue(const Index: TListSize): Boolean;
begin
  Result := Index < KeyValueCount;
  if not Result then
    WritelnWarning('Not enough key values for interpolator %d', [NiceName]);
end;

procedure TAbstractCommonInterpolatorNode
  {$ifndef FPC}<TOutputField>{$endif} .
  EventSet_FractionReceive(const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
var
  T: Single;
  KeyCount, Range: Integer;
  TempFieldIndex: TListSize;
  OutputField: TOutputField;
begin
  if not FEventValue_Changed.SendNeeded then Exit;

  KeyCount := FdKey.Items.Count;
  if KeyCount = 0 then
  begin
    { Interpolator nodes containing no keys in the key field
       shall not produce any events.

       Moreover, in case of array interpolators (like TColorInterpolatorNode)
       the "div KeyCount" used to calculate FrameValues would cause errors. }
    Exit;
  end;

  { FTempFields serves as a pool of ready field instances created by CreateOutputField.
    It is expanded when necessary. }
  TempFieldIndex := FTempFieldsUsed;
  Inc(FTempFieldsUsed);
  try
    if FTempFieldsUsed > Length(FTempFields) then
    begin
      SetLength(FTempFields, FTempFieldsUsed);
      {$ifndef FPC} Assert(FTempFields[TempFieldIndex] = nil); {$endif}
      FTempFields[TempFieldIndex] := CreateOutputField;
    end;
    OutputField := FTempFields[TempFieldIndex];
    {$ifndef FPC} Assert(OutputField <> nil); {$endif}

    Assert(Value is TSFFloat, 'EventSet_FractionReceive can only process SFFloat input events');
    Assert(not IsNan((Value as TSFFloat).Value), 'EventSet_FractionReceive cannot handle fraction = NaN');
    Range := KeyRange((Value as TSFFloat).Value, T);
    if Range = 0 then
    begin
      if not CheckKeyValue(0) then Exit;
      InterpolatorSet(OutputField, 0);
    end else
    if Range = KeyCount then
    begin
      if not CheckKeyValue(KeyCount - 1) then Exit;
      InterpolatorSet(OutputField, KeyCount - 1);
    end else
    begin
      case Interpolation of
        inLinear:
          begin
            if not CheckKeyValue(Range) then Exit;
            InterpolatorLerp(OutputField, T, Range - 1, Range);
          end;
        inStep:
          begin
            if not CheckKeyValue(Range - 1) then Exit;
            InterpolatorSet(OutputField, Range - 1);
          end;
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('TAbstractCommonInterpolatorNode:Interpolation?');
        {$endif}
      end;
    end;

    OutputField.InternalPartialSend := Value.InternalPartialSend;
    FEventValue_Changed.Send(OutputField, Time);
  finally
    Dec(FTempFieldsUsed);
  end;
end;

{ TColorInterpolatorNode ----------------------------------------------------- }

function TColorInterpolatorNode.KeyValueCount: TListSize;
begin
  Result := FdKeyValue.Items.Count;
end;

procedure TColorInterpolatorNode.InterpolatorLerp(const Target: TSFVec3f;
  const A: Single; const Index1, Index2: Integer);
begin
  Target.Value := LerpRgbInHsv(A,
    FdKeyValue.Items.List^[Index1],
    FdKeyValue.Items.List^[Index2]);
end;

procedure TColorInterpolatorNode.InterpolatorSet(const Target: TSFVec3f; const Index: Integer);
begin
  Target.Value := FdKeyValue.Items.List^[Index];
end;

function TColorInterpolatorNode.CreateOutputField: TSFVec3f;
begin
  // We pass Self as ParentNode to
  // TKeyValue.CreateUndefined.
  // This is not required, ParentNode could be @nil and everything
  // will work Ok, but setting ParentNode allows InstantReality Logger node
  // to display "sending node" information, useful for debugging.

  Result := TSFColor.CreateUndefined(Self, false, '');
end;

{ TColorSetInterpolatorNode -------------------------------------------------- }

function TColorSetInterpolatorNode.FrameValues: TListSize;
begin
  Result := FdKeyValue.Count div FdKey.Count;
end;

function TColorSetInterpolatorNode.KeyValueCount: TListSize;
begin
  Result := FdKeyValue.Items.Count div FrameValues;
end;

procedure TColorSetInterpolatorNode.InterpolatorLerp(const Target: TMFColor; const A: Single;
  const Index1, Index2: Integer);
var
  FV: TListSize;
begin
  FV := FrameValues;
  Target.Items.AssignLerpRgbInHsv(A, FdKeyValue.Items, FdKeyValue.Items,
    Index1 * FV, Index2 * FV, FrameValues);
end;

procedure TColorSetInterpolatorNode.InterpolatorSet(const Target: TMFColor; const Index: Integer);
var
  FV: TListSize;
begin
  FV := FrameValues;
  Target.Items.Clear;
  Target.Items.AddSubRange(FdKeyValue.Items, Index * FV, FV);
end;

function TColorSetInterpolatorNode.CreateOutputField: TMFColor;
begin
  Result := TMFColor.CreateUndefined(Self, false, '');
end;

{ TCoordinateInterpolatorNode ---------------------------------------------------- }

function TCoordinateInterpolatorNode.FrameValues: TListSize;
begin
  Result := FdKeyValue.Count div FdKey.Count;
end;

function TCoordinateInterpolatorNode.KeyValueCount: TListSize;
begin
  Result := FdKeyValue.Items.Count div FrameValues;
end;

procedure TCoordinateInterpolatorNode.InterpolatorLerp(const Target: TMFVec3f; const A: Single;
  const Index1, Index2: Integer);
var
  FV: TListSize;
begin
  FV := FrameValues;
  Target.Items.AssignLerpRange(A, FdKeyValue.Items, FdKeyValue.Items,
    Index1 * FV, Index2 * FV, FrameValues);
end;

procedure TCoordinateInterpolatorNode.InterpolatorSet(const Target: TMFVec3f; const Index: Integer);
var
  FV: TListSize;
begin
  FV := FrameValues;
  Target.Items.Clear;
  Target.Items.AddSubRange(FdKeyValue.Items, Index * FV, FV);
end;

function TCoordinateInterpolatorNode.CreateOutputField: TMFVec3f;
begin
  Result := TMFVec3f.CreateUndefined(Self, false, '');
end;

{ TCoordinateInterpolator2DNode ---------------------------------------------------- }

function TCoordinateInterpolator2DNode.FrameValues: TListSize;
begin
  Result := FdKeyValue.Count div FdKey.Count;
end;

function TCoordinateInterpolator2DNode.KeyValueCount: TListSize;
begin
  Result := FdKeyValue.Items.Count div FrameValues;
end;

procedure TCoordinateInterpolator2DNode.InterpolatorLerp(const Target: TMFVec2f; const A: Single;
  const Index1, Index2: Integer);
var
  FV: TListSize;
begin
  FV := FrameValues;
  Target.Items.AssignLerpRange(A, FdKeyValue.Items, FdKeyValue.Items,
    Index1 * FV, Index2 * FV, FrameValues);
end;

procedure TCoordinateInterpolator2DNode.InterpolatorSet(const Target: TMFVec2f; const Index: Integer);
var
  FV: TListSize;
begin
  FV := FrameValues;
  Target.Items.Clear;
  Target.Items.AddSubRange(FdKeyValue.Items, Index * FV, FV);
end;

function TCoordinateInterpolator2DNode.CreateOutputField: TMFVec2f;
begin
  Result := TMFVec2f.CreateUndefined(Self, false, '');
end;

{ TNormalInterpolatorNode -------------------------------------------------- }

function TNormalInterpolatorNode.FrameValues: TListSize;
begin
  Result := FdKeyValue.Count div FdKey.Count;
end;

function TNormalInterpolatorNode.KeyValueCount: TListSize;
begin
  Result := FdKeyValue.Items.Count div FrameValues;
end;

procedure TNormalInterpolatorNode.InterpolatorLerp(const Target: TMFVec3f; const A: Single;
  const Index1, Index2: Integer);
var
  FV: TListSize;
begin
  FV := FrameValues;
  Target.Items.AssignLerpNormalize(A, FdKeyValue.Items, FdKeyValue.Items,
    Index1 * FV, Index2 * FV, FrameValues);
end;

procedure TNormalInterpolatorNode.InterpolatorSet(const Target: TMFVec3f; const Index: Integer);
var
  FV: TListSize;
begin
  FV := FrameValues;
  Target.Items.Clear;
  Target.Items.AddSubRange(FdKeyValue.Items, Index * FV, FV);
end;

function TNormalInterpolatorNode.CreateOutputField: TMFVec3f;
begin
  Result := TMFVec3f.CreateUndefined(Self, false, '');
end;

{ TOrientationInterpolatorNode -------------------------------------------------- }

function TOrientationInterpolatorNode.KeyValueCount: TListSize;
begin
  Result := FdKeyValue.Items.Count;
end;

procedure TOrientationInterpolatorNode.InterpolatorLerp(const Target: TSFRotation;
  const A: Single; const Index1, Index2: Integer);
var
  Q1, Q2, QResult: TQuaternion;
begin
  { In my tests, NLerp works as good as SLerp here.
    Although NLerp doesn't guarantee constant velocity, and should be
    faster --- neither of these qualities seem to be noticeable
    in practice. }

  if KeyValueQuaternions then
  begin
    // interpolate on quaternions, assuming our inputs are unit quaternions
    Q1.Data.Vector4 := FdKeyValue.Items.List^[Index1];
    Q2.Data.Vector4 := FdKeyValue.Items.List^[Index2];
    QResult := SLerp(A, Q1, Q2);
    Target.Value := QResult.ToAxisAngle;
  end else
  begin
    Target.Value := SLerp(A,
      FdKeyValue.Items.List^[Index1],
      FdKeyValue.Items.List^[Index2]);
  end;
end;

procedure TOrientationInterpolatorNode.InterpolatorSet(const Target: TSFRotation; const Index: Integer);
begin
  if KeyValueQuaternions then
    Target.Value := Quaternion(FdKeyValue.Items.List^[Index]).ToAxisAngle
  else
    Target.Value := FdKeyValue.Items.List^[Index];
end;

function TOrientationInterpolatorNode.CreateOutputField: TSFRotation;
begin
  Result := TSFRotation.CreateUndefined(Self, false, '');
end;

function TOrientationInterpolatorNode.Interpolate(const TimeKey: Single): TVector4;
var
  T: Single;
  KeyValue: TVector4List;
  Range: Integer;
begin
  if FdKey.Items.Count = 0 then
  begin
    WritelnWarning('TOrientationInterpolatorNode has no keys, cannot interpolate, returning rotation = zero');
    Exit(TVector4.Zero);
  end;

  Range := KeyRange(TimeKey, T);
  KeyValue := FdKeyValue.Items;

  if KeyValueQuaternions then
  begin
    if Range = 0 then
      Result := Quaternion(KeyValue[0]).ToAxisAngle
    else
    if Range = KeyValue.Count then
      Result := Quaternion(KeyValue[KeyValue.Count - 1]).ToAxisAngle
    else
      // SLerp, like TOrientationInterpolatorNode.InterpolatorLerp
      Result := SLerp(T, Quaternion(KeyValue[Range - 1]), Quaternion(KeyValue[Range])).ToAxisAngle;
  end else
  begin
    if Range = 0 then
      Result := KeyValue[0]
    else
    if Range = KeyValue.Count then
      Result := KeyValue[KeyValue.Count - 1]
    else
      // SLerp, like TOrientationInterpolatorNode.InterpolatorLerp
      Result := SLerp(T, KeyValue[Range - 1], KeyValue[Range]);
  end;
end;

{ TPositionInterpolatorNode -------------------------------------------------- }

function TPositionInterpolatorNode.KeyValueCount: TListSize;
begin
  Result := FdKeyValue.Items.Count;
end;

procedure TPositionInterpolatorNode.InterpolatorLerp(const Target: TSFVec3f;
  const A: Single; const Index1, Index2: Integer);
begin
  Target.Value := Lerp(A,
    FdKeyValue.Items.List^[Index1],
    FdKeyValue.Items.List^[Index2]);
end;

procedure TPositionInterpolatorNode.InterpolatorSet(const Target: TSFVec3f; const Index: Integer);
begin
  Target.Value := FdKeyValue.Items.List^[Index];
end;

function TPositionInterpolatorNode.CreateOutputField: TSFVec3f;
begin
  Result := TSFVec3f.CreateUndefined(Self, false, '');
end;

function TPositionInterpolatorNode.Interpolate(const TimeKey: Single): TVector3;
var
  T: Single;
  KeyValue: TVector3List;
  Range: Integer;
begin
  if FdKey.Items.Count = 0 then
  begin
    WritelnWarning('TPositionInterpolatorNode has no keys, cannot interpolate, returning position = zero');
    Exit(TVector3.Zero);
  end;

  Range := KeyRange(TimeKey, T);
  KeyValue := FdKeyValue.Items;

  if Range = 0 then
    Result := KeyValue[0]
  else
  if Range = KeyValue.Count then
    Result := KeyValue[KeyValue.Count - 1]
  else
    Result := TVector3.Lerp(T, KeyValue[Range - 1], KeyValue[Range]);
end;

{ TPositionInterpolator2DNode -------------------------------------------------- }

function TPositionInterpolator2DNode.KeyValueCount: TListSize;
begin
  Result := FdKeyValue.Items.Count;
end;

procedure TPositionInterpolator2DNode.InterpolatorLerp(const Target: TSFVec2f;
  const A: Single; const Index1, Index2: Integer);
begin
  Target.Value := Lerp(A,
    FdKeyValue.Items.List^[Index1],
    FdKeyValue.Items.List^[Index2]);
end;

procedure TPositionInterpolator2DNode.InterpolatorSet(const Target: TSFVec2f; const Index: Integer);
begin
  Target.Value := FdKeyValue.Items.List^[Index];
end;

function TPositionInterpolator2DNode.CreateOutputField: TSFVec2f;
begin
  Result := TSFVec2f.CreateUndefined(Self, false, '');
end;

{ TScalarInterpolatorNode -------------------------------------------------- }

function TScalarInterpolatorNode.KeyValueCount: TListSize;
begin
  Result := FdKeyValue.Items.Count;
end;

procedure TScalarInterpolatorNode.InterpolatorLerp(const Target: TSFFloat;
  const A: Single; const Index1, Index2: Integer);
begin
  Target.Value := Lerp(A,
    FdKeyValue.Items.List^[Index1],
    FdKeyValue.Items.List^[Index2]);
end;

procedure TScalarInterpolatorNode.InterpolatorSet(const Target: TSFFloat; const Index: Integer);
begin
  Target.Value := FdKeyValue.Items.List^[Index];
end;

function TScalarInterpolatorNode.CreateOutputField: TSFFloat;
begin
  Result := TSFFloat.CreateUndefined(Self, false, '');
end;

{ TVectorInterpolatorNode ---------------------------------------------------- }

function TVectorInterpolatorNode.FrameValues: TListSize;
begin
  Result := FdKeyValue.Count div FdKey.Count;
end;

function TVectorInterpolatorNode.KeyValueCount: TListSize;
begin
  Result := FdKeyValue.Items.Count div FrameValues;
end;

procedure TVectorInterpolatorNode.InterpolatorLerp(const Target: TMFFloat; const A: Single;
  const Index1, Index2: Integer);
var
  FV: TListSize;
begin
  FV := FrameValues;
  Target.Items.AssignLerpRange(A, FdKeyValue.Items, FdKeyValue.Items,
    Index1 * FV, Index2 * FV, FrameValues);
end;

procedure TVectorInterpolatorNode.InterpolatorSet(const Target: TMFFloat; const Index: Integer);
var
  FV: TListSize;
begin
  FV := FrameValues;
  Target.Items.Clear;
  Target.Items.AddSubRange(FdKeyValue.Items, Index * FV, FV);
end;

function TVectorInterpolatorNode.CreateOutputField: TMFFloat;
begin
  Result := TMFFloat.CreateUndefined(Self, false, '');
end;

{ registration --------------------------------------------------------------- }

procedure RegisterInterpolationNodes;
begin
  NodesManager.RegisterNodeClasses([
    TColorInterpolatorNode,
    TColorSetInterpolatorNode,
    TCoordinateInterpolatorNode,
    TCoordinateInterpolator2DNode,
    TEaseInEaseOutNode,
    TNormalInterpolatorNode,
    TOrientationInterpolatorNode,
    TPositionInterpolatorNode,
    TPositionInterpolator2DNode,
    TScalarInterpolatorNode,
    TSplinePositionInterpolatorNode,
    TSplinePositionInterpolator2DNode,
    TSplineScalarInterpolatorNode,
    TSquadOrientationInterpolatorNode,
    TVectorInterpolatorNode
  ]);
end;

{$endif read_implementation}
