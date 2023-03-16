{%MainUnit x3dloadinternalspine.pas}
{
  Copyright 2014-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Animation utilities. }

{$ifdef read_interface}
{$endif}

{$ifdef read_implementation}

const
  SSpine1KeyFrameWarning = 'Timeline on %s "%s" (animation of %s) has only 1 keyframe. We support it in a consistent way (1 keyframe means that the state is constant), but Spine shows such timeline inconsistently. To avoid confusion, set at least 2 keyframes on every timeline.';
  // SSpineMultipleTimelinesWarning = 'Multiple timelines affect %s "%s" animation of %s';

{ Safer version than CastleUtils.MapRangeTo01 that doesn't account for SourceBegin=SourceEnd case. }
function MapRangeTo01(const SourceVal, SourceBegin, SourceEnd: Single): Single;
begin
  if SameValue(SourceEnd, SourceBegin) then
    Result := 0 // whatever result is valid, just avoid returning NaN
  else
    Result := (SourceVal - SourceBegin) / (SourceEnd - SourceBegin);
end;

{ Read Bezier curve from Spine file.

  Spine docs that curve count should be 4,
  http://esotericsoftware.com/spine-json-format
  """A Bézier curve has 4 elements which define the control points: cx1, cy1, cx2, cy2. The X axis is from 0 to 1 and represents the percent of time between the two keyframes. The Y axis is from 0 to 1 and represents the percent of the difference between the keyframe's values."""
  But in reality it is now a **multiple of 4** (see below).

  @param(NormalizeKeyValues

    Since around Spine 4.0, Spine writes actual times in file (not times normalized to 0..1 range
    between previous and next frames) and actual values in file (not values normalized to 0..1 range
    between previous and next values).

    For now, we counteract this, by normalizing them back into 0..1 range,
    as that's what our TCubicBezierPositionInterpolatorNode and friends expect.
    TODO: In the future we should allow TCubicBezierPositionInterpolatorNode to use unnormalized
    values.

    The PreviousTime, NextTime, PreviousValue, NextValue are used for this normalization.
  )

  @param(VectorComponents

    Since around Spine 4.0, Spine write separate curve values for each vector component.
    E.g. to animate 2D translation, Spine actually writes 2 curves.
    To animate RGBA animation, Spine actually writes 4 curves.

    This is not documented yet in Spine docs.

    TODO: We do not support it yet, instead we check that keys are equal,
    and we average the values (percentage of values, actually;
    NormalizeKeyValues must be true for this).
    For a full support, we'd have to extend our interpolators to account for this.

    We only use VectorComponents from PreviousValue and NextValue.
    E.g. when VectorComponents = 2, we only look at PreviousValue.X, PreviousValue.Y,
    and we ignore PreviousValue.Z, PreviousValue.W.
  )
}
function ReadCurveControlPoints(const OwnerName: String; const Json: TJSONObject;
  const NormalizeKeyValues: Boolean;
  const VectorComponents: Cardinal;
  const PreviousTime, NextTime: Single;
  const PreviousValue, NextValue: TVector4): TVector4;

  function NormalizeTime(const T: Single): Single;
  begin
    if NormalizeKeyValues then
      Result := MapRangeTo01(T, PreviousTime, NextTime)
    else
      Result := T;
  end;

  function NormalizeValue(const V: Single; const Component: Cardinal): Single;
  begin
    if NormalizeKeyValues then
      Result := MapRangeTo01(V, PreviousValue[Component], NextValue[Component])
    else
      Result := V;
  end;

var
  CurveJson: TJSONData;
  CurveJsonArray: TJSONArray;
  C, J: Integer;
  NewControlPoints: TVector4;
begin
  Result := Vector4(0, 0, 1, 1); // default value, in case curve changes from "curve" to default linear interpolation
  CurveJson := Json.Find('curve');
  if CurveJson <> nil then
  begin
    if CurveJson is TJSONArray then
    begin
      CurveJsonArray := TJSONArray(CurveJson);
      C := TJSONArray(CurveJson).Count;
      if C = 4 then
      begin
        Result.X := NormalizeTime (CurveJsonArray.Floats[0]);
        Result.Y := NormalizeValue(CurveJsonArray.Floats[1], 0);
        Result.Z := NormalizeTime (CurveJsonArray.Floats[2]);
        Result.W := NormalizeValue(CurveJsonArray.Floats[3], 0);
      end else
      if C = 4 * VectorComponents then
      begin
        Result.X := NormalizeTime (CurveJsonArray.Floats[0]);
        Result.Y := NormalizeValue(CurveJsonArray.Floats[1], 0);
        Result.Z := NormalizeTime (CurveJsonArray.Floats[2]);
        Result.W := NormalizeValue(CurveJsonArray.Floats[3], 0);
        for J := 1 to VectorComponents - 1 do
        begin
          NewControlPoints.X := NormalizeTime (CurveJsonArray.Floats[J * 4 + 0]);
          NewControlPoints.Y := NormalizeValue(CurveJsonArray.Floats[J * 4 + 1], J);
          NewControlPoints.Z := NormalizeTime (CurveJsonArray.Floats[J * 4 + 2]);
          NewControlPoints.W := NormalizeValue(CurveJsonArray.Floats[J * 4 + 3], J);
          if (not SameValue(Result.X, NewControlPoints.X)) or
             (not SameValue(Result.Z, NewControlPoints.Z)) then
          begin
            WritelnWarning('Spine', 'Curve interpolation of "%s" has different keys for %d components', [
              OwnerName,
              VectorComponents
            ]);
            Exit;
          end;
          { // Avoid this warning, too spamming.
          if (not SameValue(Result.Y, NewControlPoints.Y)) or
             (not SameValue(Result.W, NewControlPoints.W)) then
            WritelnWarning('Spine', 'Curve interpolation of "%s" has different values for %d components. Averaging.', [
              OwnerName,
              VectorComponents
            ]);
          }
          Result.Y := Result.Y + NewControlPoints.Y;
          Result.W := Result.W + NewControlPoints.Y;
        end;
        Result.Y := Result.Y / VectorComponents;
        Result.W := Result.W / VectorComponents;
      end else
      begin
        WritelnWarning('Spine', 'Curve interpolation of "%s" is an array with %d elements (expected 4 or %d)', [
          OwnerName,
          C,
          4 * VectorComponents
        ]);
      end;
    end;

    { For now, silently ignore warning that we don't handle curve type
      'stepped'. Spine optimizes it's curves by using 'stepped' where
      previous and next values are equal, so this is common, and we would
      flood the warnings console for many models because of this (while in fact
      models are handled Ok since 'stepped' is only for optimization). }
    { else
    if CurveJson.AsString <> 'linear' then
      WritelnWarning('Spine', 'Found "' + CurveJson.AsString + '" interpolation type on slot timeline of slot ' + Slot.Name + ', we do not support this interpolation type');
    }
  end;
end;

{ Used by deform. In Spine 4, only time is required to normalize back to 0..1 range,
  while value is still in 0..1 range }
function ReadCurveControlPointsTimeOnly(const Json: TJSONObject;
  const NormalizeKeyValues: Boolean;
  const PreviousTime, NextTime: Single): TVector4;

  function NormalizeTime(const T: Single): Single;
  begin
    if NormalizeKeyValues then
      Result := MapRangeTo01(T, PreviousTime, NextTime)
    else
      Result := T;
  end;

var
  CurveJson: TJSONData;
  CurveJsonArray: TJSONArray;
begin
  Result := Vector4(0, 0, 1, 1); // default value, in case curve changes from "curve" to default linear interpolation
  CurveJson := Json.Find('curve');
  if CurveJson <> nil then
  begin
    if CurveJson is TJSONArray then
    begin
      CurveJsonArray := TJSONArray(CurveJson);
      Result.X := NormalizeTime (CurveJsonArray.Floats[0]);
      Result.Y := CurveJsonArray.Floats[1];
      Result.Z := NormalizeTime (CurveJsonArray.Floats[2]);
      Result.W := CurveJsonArray.Floats[3];
    end;
  end;
end;

{$endif}
