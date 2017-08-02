{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ CastleScript vector and matrix types and built-in functions. }
unit CastleScriptVectors;

{$I castleconf.inc}

interface

uses CastleVectors, CastleScript;

type
  TCasScriptVector = class;
  TCasScriptVectorD = class;

  generic TCasScriptVec<
    TVectorXxx,
    TCasScriptVectorFunXxx> = class(TCasScriptValue)
  private
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMax(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMin(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleVector(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSqrLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorDot(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class function VectorGetCount: Integer;
    class procedure RegisterFunctions;
  private
    FValue: TVectorXxx;
    procedure SetValue(const AValue: TVectorXxx);
  public
    property Value: TVectorXxx read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptVec2f = class(specialize TCasScriptVec<
    TVector2,
    TCasScriptVector>)
  end;

  TCasScriptVec3f = class(specialize TCasScriptVec<
    TVector3,
    TCasScriptVector>)
  private
    class procedure HandleVectorCross(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleGrayscale(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  end;

  TCasScriptVec4f = class(specialize TCasScriptVec<
    TVector4,
    TCasScriptVector>)
  private
    class procedure HandleOrientationFromDirectionUp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleRotateCore(
      const Rotation: TVector4; const Point: TVector3;
      var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleRotate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleOrientationToDirection(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleOrientationToUp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleSlerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  end;

  TCasScriptVec2d = class(specialize TCasScriptVec<
    TVector2Double,
    TCasScriptVectorD>)
  end;

  TCasScriptVec3d = class(specialize TCasScriptVec<
    TVector3Double,
    TCasScriptVectorD>)
  private
    class procedure HandleVectorCross(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  end;

  TCasScriptVec4d = class(specialize TCasScriptVec<
    TVector4Double,
    TCasScriptVectorD>)
  end;

  generic TCasScriptMatrix<
    TCasScriptVecXxx,
    TMatrixXxx,
    TVectorXxx> = class(TCasScriptValue)
  private
    class procedure HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class procedure HandleMatrix(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleMatrixGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);

    class function MatrixGetCount: Integer;
    class procedure RegisterFunctions;
  private
    FValue: TMatrixXxx;
    procedure SetValue(const AValue: TMatrixXxx);
  public
    property Value: TMatrixXxx read FValue write SetValue;

    procedure AssignValue(Source: TCasScriptValue); override;
  end;

  TCasScriptMatrix3f = class(specialize TCasScriptMatrix<
    TCasScriptVec3f,
    TMatrix3,
    TVector3>)
  end;

  TCasScriptMatrix4f = class(specialize TCasScriptMatrix<
    TCasScriptVec4f,
    TMatrix4,
    TVector4>)
  end;

  TCasScriptMatrix3d = class(specialize TCasScriptMatrix<
    TCasScriptVec3d,
    TMatrix3Double,
    TVector3Double>)
  end;

  TCasScriptMatrix4d = class(specialize TCasScriptMatrix<
    TCasScriptVec4d,
    TMatrix4Double,
    TVector4Double>)
  end;

  TCasScriptVector = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptVectorD = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptVectorGet = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptVectorSet = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
    class function ArgumentMustBeAssignable(const Index: Integer): boolean; override;
  end;

  TCasScriptVectorGetCount = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptVectorLength = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptVectorSqrLength = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptVectorDot = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptVectorCross = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptGrayscale = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptOrientationFromDirectionUp = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptRotate = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptOrientationToDirection = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptOrientationToUp = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptSlerp = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptMatrixFun = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptMatrixGet = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

  TCasScriptMatrixSet = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
    class function ArgumentMustBeAssignable(const Index: Integer): boolean; override;
  end;

  TCasScriptMatrixGetCount = class(TCasScriptFunction)
  public
    class function ShortName: string; override;
  end;

implementation

uses CastleScriptCoreFunctions, CastleUtils, CastleLog, CastleCameras, CastleQuaternions,
  CastleColors;

{ TCasScriptVec ---------------------------------------------------------- }

class procedure TCasScriptVec.HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
  { The function allows only >= 1 arguments, and this handler is
    registered only for TCasScriptVec values, so we can safely take
    the first arg as TCasScriptVec. }
  TCasScriptVec(AResult).Value := TCasScriptVec(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TCasScriptVec(AResult).Value :=
      TCasScriptVec(AResult).Value +
      TCasScriptVec(Arguments[I]).Value;
end;

class procedure TCasScriptVec.HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
  TCasScriptVec(AResult).Value := TCasScriptVec(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TCasScriptVec(AResult).Value :=
      TCasScriptVec(AResult).Value - TCasScriptVec(Arguments[I]).Value;
end;

class procedure TCasScriptVec.HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
  TCasScriptVec(AResult).Value := -TCasScriptVec(Arguments[0]).Value;
end;

class procedure TCasScriptVec.HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptBoolean);
  TCasScriptBoolean(AResult).Value :=
    TVectorXxx.PerfectlyEquals(
      TCasScriptVec(Arguments[0]).Value,
      TCasScriptVec(Arguments[1]).Value);
end;

class procedure TCasScriptVec.HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptBoolean);
  TCasScriptBoolean(AResult).Value :=
    not TVectorXxx.PerfectlyEquals(
      TCasScriptVec(Arguments[0]).Value,
      TCasScriptVec(Arguments[1]).Value);
end;

class procedure TCasScriptVec.HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
  if Arguments[0] is TCasScriptFloat then
  begin
    TCasScriptVec(AResult).Value :=
      TCasScriptVec(Arguments[1]).Value *
      TCasScriptFloat(Arguments[0]).Value;
  end else
  begin
    TCasScriptVec(AResult).Value :=
      TCasScriptVec(Arguments[0]).Value *
      TCasScriptFloat(Arguments[1]).Value;
  end;
end;

class procedure TCasScriptVec.HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
  TCasScriptVec(AResult).Value :=
    TCasScriptVec(Arguments[0]).Value *
    1/TCasScriptFloat(Arguments[1]).Value;
end;

class procedure TCasScriptVec.HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
  TCasScriptVec(AResult).Value := TVectorXxx.Lerp(
    TCasScriptFloat(Arguments[0]).Value,
    TCasScriptVec(Arguments[1]).Value,
    TCasScriptVec(Arguments[2]).Value);
end;

class procedure TCasScriptVec.HandleMax(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
  for I := 0 to VectorGetCount - 1 do
    TCasScriptVec(AResult).FValue[I] :=
      Max( TCasScriptVec(Arguments[0]).FValue[I],
           TCasScriptVec(Arguments[1]).FValue[I] );
  TCasScriptVec(AResult).ValueAssigned := true;
end;

class procedure TCasScriptVec.HandleMin(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
  for I := 0 to VectorGetCount - 1 do
    TCasScriptVec(AResult).FValue[I] :=
      Min( TCasScriptVec(Arguments[0]).FValue[I],
           TCasScriptVec(Arguments[1]).FValue[I] );
  TCasScriptVec(AResult).ValueAssigned := true;
end;

class procedure TCasScriptVec.HandleVector(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
  for I := 0 to VectorGetCount - 1 do
    TCasScriptVec(AResult).FValue[I] := TCasScriptFloat(Arguments[I]).Value;
  TCasScriptVec(AResult).ValueAssigned := true;
end;

class procedure TCasScriptVec.HandleVectorGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);

  Index := TCasScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, VectorGetCount - 1) then
    raise ECasScriptError.CreateFmt('Invalid index %d for vector_get on %d-element vector',
      [Index, VectorGetCount]);

  TCasScriptFloat(AResult).Value := TCasScriptVec(Arguments[0]).Value[Index];
end;

class procedure TCasScriptVec.HandleVectorSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  Index := TCasScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, VectorGetCount - 1) then
    raise ECasScriptError.CreateFmt('Invalid index %d for vector_set on %d-element vector',
      [Index, VectorGetCount]);

  TCasScriptVec(Arguments[0]).FValue[Index] := TCasScriptFloat(Arguments[2]).Value;
  TCasScriptVec(Arguments[0]).ValueAssigned := true;

  AResult := Arguments[0];
end;

class procedure TCasScriptVec.HandleVectorGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptInteger);
  TCasScriptInteger(AResult).Value := VectorGetCount;
end;

class procedure TCasScriptVec.HandleVectorLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
  TCasScriptFloat(AResult).Value := TCasScriptVec(Arguments[0]).Value.Length;
end;

class procedure TCasScriptVec.HandleVectorSqrLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
  TCasScriptFloat(AResult).Value := TCasScriptVec(Arguments[0]).Value.LengthSqr;
end;

class procedure TCasScriptVec.HandleVectorDot(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
  TCasScriptFloat(AResult).Value :=
    TVectorXxx.DotProduct(
      TCasScriptVec(Arguments[0]).Value,
      TCasScriptVec(Arguments[1]).Value );
end;

procedure TCasScriptVec.AssignValue(Source: TCasScriptValue);
begin
  if Source is TCasScriptVec then
    Value := TCasScriptVec(Source).Value else
    raise ECasScriptAssignError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

procedure TCasScriptVec.SetValue(const AValue: TVectorXxx);
begin
  FValue := AValue;
  ValueAssigned := true;
end;

class function TCasScriptVec.VectorGetCount: Integer;
begin
  Result := High(TVectorXxx.TIndex) + 1;
end;

class procedure TCasScriptVec.RegisterFunctions;
var
  VectorArgClasses: array of TCasScriptValueClass;
  I: Integer;
begin
  { functions from CastleScriptCoreFunctions }
  FunctionHandlers.RegisterHandler(@HandleAdd, TCasScriptAdd, [TCasScriptVec], true);
  FunctionHandlers.RegisterHandler(@HandleSubtract, TCasScriptSubtract, [TCasScriptVec], true);
  FunctionHandlers.RegisterHandler(@HandleNegate, TCasScriptNegate, [TCasScriptVec], false);
  FunctionHandlers.RegisterHandler(@HandleEqual, TCasScriptEqual, [TCasScriptVec, TCasScriptVec], false);
  FunctionHandlers.RegisterHandler(@HandleNotEqual, TCasScriptNotEqual, [TCasScriptVec, TCasScriptVec], false);

  FunctionHandlers.RegisterHandler(@HandleMultiply, TCasScriptMultiply, [TCasScriptVec, TCasScriptFloat], false);
  FunctionHandlers.RegisterHandler(@HandleMultiply, TCasScriptMultiply, [TCasScriptFloat, TCasScriptVec], false);
  FunctionHandlers.RegisterHandler(@HandleDivide, TCasScriptDivide, [TCasScriptVec, TCasScriptFloat], false);
  FunctionHandlers.RegisterHandler(@HandleLerp, TCasScriptLerp, [TCasScriptFloat, TCasScriptVec, TCasScriptVec], false);

  FunctionHandlers.RegisterHandler(@HandleMax, TCasScriptMax, [TCasScriptVec, TCasScriptVec], false);
  FunctionHandlers.RegisterHandler(@HandleMin, TCasScriptMin, [TCasScriptVec, TCasScriptVec], false);

  { functions from CastleScriptVectors }
  SetLength(VectorArgClasses, VectorGetCount);
  for I := 0 to VectorGetCount - 1 do
    VectorArgClasses[I] := TCasScriptFloat;
  FunctionHandlers.RegisterHandler(@HandleVector, TCasScriptVectorFunXxx, VectorArgClasses, false);

  FunctionHandlers.RegisterHandler(@HandleVectorGet, TCasScriptVectorGet, [TCasScriptVec, TCasScriptInteger], false);
  FunctionHandlers.RegisterHandler(@HandleVectorSet, TCasScriptVectorSet, [TCasScriptVec, TCasScriptInteger, TCasScriptFloat], false);
  FunctionHandlers.RegisterHandler(@HandleVectorGetCount, TCasScriptVectorGetCount, [TCasScriptVec], false);
  FunctionHandlers.RegisterHandler(@HandleVectorLength, TCasScriptVectorLength, [TCasScriptVec], false);
  FunctionHandlers.RegisterHandler(@HandleVectorSqrLength, TCasScriptVectorSqrLength, [TCasScriptVec], false);
  FunctionHandlers.RegisterHandler(@HandleVectorDot, TCasScriptVectorDot, [TCasScriptVec, TCasScriptVec], false);
end;

{ TCasScriptVec3f ------------------------------------------------------------ }

class procedure TCasScriptVec3f.HandleVectorCross(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVec3f);
  TCasScriptVec3f(AResult).Value := TVector3.CrossProduct(
    TCasScriptVec3f(Arguments[0]).Value,
    TCasScriptVec3f(Arguments[1]).Value );
end;

class procedure TCasScriptVec3f.HandleGrayscale(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
  TCasScriptFloat(AResult).Value :=
    GrayscaleValue( TCasScriptVec3f(Arguments[0]).Value );
end;

{ TCasScriptVec4f ------------------------------------------------------------ }

class procedure TCasScriptVec4f.HandleOrientationFromDirectionUp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Dir, Up: TVector3;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVec4f);

  Dir := TCasScriptVec3f(Arguments[0]).Value;
  Up := TCasScriptVec3f(Arguments[1]).Value;
  MakeVectorsOrthoOnTheirPlane(Up, Dir);
  { no need to normalize Dir, Up here (CamDirUp2Orient will do it) }

  TCasScriptVec4f(AResult).Value := CamDirUp2Orient(Dir, Up);
end;

class procedure TCasScriptVec4f.HandleRotateCore(
  const Rotation: TVector4; const Point: TVector3;
  var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Axis: TVector3 absolute Rotation;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVec3f);

  if not Axis.IsZero then
    TCasScriptVec3f(AResult).Value := RotatePointAroundAxisRad(
      Rotation[3], Point, Axis) else
  begin
    { Safeguard against rotation around zero vector, which produces unpredictable
      results (actually, Result would be filled with Nan values).
      VRML/X3D specs says that SFRotation should always specify a normalized vector. }
    TCasScriptVec3f(AResult).Value := Point;
    WritelnWarning('CastleScript', 'Rotation around zero vector');
  end;
end;

class procedure TCasScriptVec4f.HandleRotate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  HandleRotateCore(
    TCasScriptVec4f(Arguments[0]).Value,
    TCasScriptVec3f(Arguments[1]).Value, AResult, ParentOfResult);
end;

class procedure TCasScriptVec4f.HandleOrientationToDirection(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  HandleRotateCore(
    TCasScriptVec4f(Arguments[0]).Value,
    DefaultCameraDirection, AResult, ParentOfResult);
end;

class procedure TCasScriptVec4f.HandleOrientationToUp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  HandleRotateCore(
    TCasScriptVec4f(Arguments[0]).Value,
    DefaultCameraUp, AResult, ParentOfResult);
end;

class procedure TCasScriptVec4f.HandleSlerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVec4f);
  TCasScriptVec4f(AResult).Value := SLerp(
    TCasScriptFloat(Arguments[0]).Value,
    TCasScriptVec4f(Arguments[1]).Value,
    TCasScriptVec4f(Arguments[2]).Value);
end;

{ TCasScriptVec3d ------------------------------------------------------------ }

class procedure TCasScriptVec3d.HandleVectorCross(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVec3d);
  TCasScriptVec3d(AResult).Value :=
    TVector3Double.CrossProduct(
      TCasScriptVec3d(Arguments[0]).Value,
      TCasScriptVec3d(Arguments[1]).Value );
end;

{ TCasScriptMatrix ---------------------------------------------------------- }

class procedure TCasScriptMatrix.HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
  { The function allows only >= 1 arguments, and this handler is
    registered only for TCasScriptMatrix values, so we can safely take
    the first arg as TCasScriptMatrix. }
  TCasScriptMatrix(AResult).Value := TCasScriptMatrix(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TCasScriptMatrix(AResult).Value :=
      TCasScriptMatrix(AResult).Value + TCasScriptMatrix(Arguments[I]).Value;
end;

class procedure TCasScriptMatrix.HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
  TCasScriptMatrix(AResult).Value := TCasScriptMatrix(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TCasScriptMatrix(AResult).Value :=
      TCasScriptMatrix(AResult).Value - TCasScriptMatrix(Arguments[I]).Value;
end;

class procedure TCasScriptMatrix.HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
  TCasScriptMatrix(AResult).Value := -TCasScriptMatrix(Arguments[0]).Value;
end;

class procedure TCasScriptMatrix.HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  if (Length(Arguments) = 2) and
     (Arguments[0] is TCasScriptFloat) and
     (Arguments[1] is TCasScriptMatrix) then
  begin
    CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
    TCasScriptMatrix(AResult).Value :=
      TCasScriptMatrix(Arguments[1]).Value *
      TCasScriptFloat(Arguments[0]).Value;
  end else
  if (Length(Arguments) = 2) and
     (Arguments[1] is TCasScriptFloat) and
     (Arguments[0] is TCasScriptMatrix) then
  begin
    CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
    TCasScriptMatrix(AResult).Value :=
      TCasScriptMatrix(Arguments[0]).Value *
      TCasScriptFloat(Arguments[1]).Value;
  end else
  if (Length(Arguments) = 2) and
     (Arguments[0] is TCasScriptMatrix) and
     (Arguments[1].InheritsFrom(TCasScriptVecXxx)) then
  begin
    CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVecXxx);
    TCasScriptVecXxx(AResult).Value :=
      TCasScriptMatrix(Arguments[0]).Value *
      TCasScriptVecXxx(Arguments[1]).Value;
  end else
  begin
    { So this is matrix * matrix... operation }
    CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
    TCasScriptMatrix(AResult).Value := TCasScriptMatrix(Arguments[0]).Value;
    for I := 1 to Length(Arguments) - 1 do
      TCasScriptMatrix(AResult).Value :=
        TCasScriptMatrix(AResult).Value *
        TCasScriptMatrix(Arguments[I]).Value;
  end;
end;

class procedure TCasScriptMatrix.HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
  TCasScriptMatrix(AResult).Value :=
    TCasScriptMatrix(Arguments[0]).Value *
    (1/TCasScriptFloat(Arguments[1]).Value);
end;

class procedure TCasScriptMatrix.HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
  TCasScriptMatrix(AResult).Value := TMatrixXxx.Lerp(
    TCasScriptFloat(Arguments[0]).Value,
    TCasScriptMatrix(Arguments[1]).Value,
    TCasScriptMatrix(Arguments[2]).Value);
end;

class procedure TCasScriptMatrix.HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptBoolean);
  TCasScriptBoolean(AResult).Value :=
    TMatrixXxx.PerfectlyEquals(
      TCasScriptMatrix(Arguments[0]).Value,
      TCasScriptMatrix(Arguments[1]).Value);
end;

class procedure TCasScriptMatrix.HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptBoolean);
  TCasScriptBoolean(AResult).Value :=
    not TMatrixXxx.PerfectlyEquals(
      TCasScriptMatrix(Arguments[0]).Value,
      TCasScriptMatrix(Arguments[1]).Value);
end;

class procedure TCasScriptMatrix.HandleMatrix(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptValueClass(ClassType));
  for I := 0 to MatrixGetCount - 1 do
    TCasScriptMatrix(AResult).FValue.Data[I] := TCasScriptVecXxx(Arguments[I]).Value.Data;
  TCasScriptMatrix(AResult).ValueAssigned := true;
end;

class procedure TCasScriptMatrix.HandleMatrixGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
  V: TVectorXxx;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVecXxx);

  Index := TCasScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, MatrixGetCount - 1) then
    raise ECasScriptError.CreateFmt('Invalid index %d for matrix_get on %d-column matrix',
      [Index, MatrixGetCount]);

  V.Data := TCasScriptMatrix(Arguments[0]).Value.Data[Index];
  TCasScriptVecXxx(AResult).Value := V;
end;

class procedure TCasScriptMatrix.HandleMatrixSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  Index := TCasScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, MatrixGetCount - 1) then
    raise ECasScriptError.CreateFmt('Invalid index %d for matrix_set on %d-column matrix',
      [Index, MatrixGetCount]);

  TCasScriptMatrix(Arguments[0]).FValue.Data[Index] := TCasScriptVecXxx(Arguments[2]).Value.Data;
  TCasScriptMatrix(Arguments[0]).ValueAssigned := true;

  AResult := Arguments[0];
end;

class procedure TCasScriptMatrix.HandleMatrixGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptInteger);
  TCasScriptInteger(AResult).Value := MatrixGetCount;
end;

procedure TCasScriptMatrix.AssignValue(Source: TCasScriptValue);
begin
  if Source is TCasScriptMatrix then
    Value := TCasScriptMatrix(Source).Value else
    raise ECasScriptAssignError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

procedure TCasScriptMatrix.SetValue(const AValue: TMatrixXxx);
begin
  FValue := AValue;
  ValueAssigned := true;
end;

class function TCasScriptMatrix.MatrixGetCount: Integer;
begin
  Result := High(TVectorXxx.TIndex) + 1;
end;

class procedure TCasScriptMatrix.RegisterFunctions;
var
  MatrixArgClasses: array of TCasScriptValueClass;
  I: Integer;
begin
  { functions from CastleScriptCoreFunctions }
  FunctionHandlers.RegisterHandler(@HandleAdd, TCasScriptAdd, [TCasScriptMatrix], true);
  FunctionHandlers.RegisterHandler(@HandleSubtract, TCasScriptSubtract, [TCasScriptMatrix], true);
  FunctionHandlers.RegisterHandler(@HandleNegate, TCasScriptNegate, [TCasScriptMatrix], false);

  FunctionHandlers.RegisterHandler(@HandleMultiply, TCasScriptMultiply, [TCasScriptMatrix, TCasScriptFloat], false);
  FunctionHandlers.RegisterHandler(@HandleMultiply, TCasScriptMultiply, [TCasScriptFloat, TCasScriptMatrix], false);
  FunctionHandlers.RegisterHandler(@HandleMultiply, TCasScriptMultiply, [TCasScriptMatrix, TCasScriptVecXxx], false);
  FunctionHandlers.RegisterHandler(@HandleMultiply, TCasScriptMultiply, [TCasScriptMatrix], true);
  FunctionHandlers.RegisterHandler(@HandleDivide, TCasScriptDivide, [TCasScriptMatrix, TCasScriptFloat], false);
  FunctionHandlers.RegisterHandler(@HandleLerp, TCasScriptLerp, [TCasScriptFloat, TCasScriptMatrix, TCasScriptMatrix], false);

  FunctionHandlers.RegisterHandler(@HandleEqual, TCasScriptEqual, [TCasScriptMatrix, TCasScriptMatrix], false);
  FunctionHandlers.RegisterHandler(@HandleNotEqual, TCasScriptNotEqual, [TCasScriptMatrix, TCasScriptMatrix], false);

  { functions from CastleScriptVectors }
  SetLength(MatrixArgClasses, MatrixGetCount);
  for I := 0 to MatrixGetCount - 1 do
    MatrixArgClasses[I] := TCasScriptVecXxx;
  FunctionHandlers.RegisterHandler(@HandleMatrix, TCasScriptMatrixFun, MatrixArgClasses, false);

  FunctionHandlers.RegisterHandler(@HandleMatrixGet, TCasScriptMatrixGet, [TCasScriptMatrix, TCasScriptInteger], false);
  FunctionHandlers.RegisterHandler(@HandleMatrixSet, TCasScriptMatrixSet, [TCasScriptMatrix, TCasScriptInteger, TCasScriptVecXxx], false);
  FunctionHandlers.RegisterHandler(@HandleMatrixGetCount, TCasScriptMatrixGetCount, [TCasScriptMatrix], false);
end;

{ TCasScriptFunction descendants --------------------------------------------- }

class function TCasScriptVector.ShortName: string;
begin
  Result := 'vector';
end;

class function TCasScriptVectorD.ShortName: string;
begin
  Result := 'vector_d';
end;

class function TCasScriptVectorGet.ShortName: string;
begin
  Result := 'vector_get';
end;

class function TCasScriptVectorSet.ShortName: string;
begin
  Result := 'vector_set';
end;

class function TCasScriptVectorSet.ArgumentMustBeAssignable(const Index: Integer): boolean;
begin
  Result := Index = 0;
end;

class function TCasScriptVectorGetCount.ShortName: string;
begin
  Result := 'vector_get_count';
end;

class function TCasScriptVectorLength.ShortName: string;
begin
  Result := 'vector_length';
end;

class function TCasScriptVectorSqrLength.ShortName: string;
begin
  Result := 'vector_sqr_length';
end;

class function TCasScriptVectorDot.ShortName: string;
begin
  Result := 'vector_dot';
end;

class function TCasScriptVectorCross.ShortName: string;
begin
  Result := 'vector_cross';
end;

class function TCasScriptGrayscale.ShortName: string;
begin
  Result := 'grayscale';
end;

class function TCasScriptOrientationFromDirectionUp.ShortName: string;
begin
  Result := 'orientation_from_direction_up';
end;

class function TCasScriptRotate.ShortName: string;
begin
  Result := 'rotate';
end;

class function TCasScriptOrientationToDirection.ShortName: string;
begin
  Result := 'orientation_to_direction';
end;

class function TCasScriptOrientationToUp.ShortName: string;
begin
  Result := 'orientation_to_up';
end;

class function TCasScriptSlerp.ShortName: string;
begin
  Result := 'slerp';
end;

{ matrix functions ----------------------------------------------------------- }

class function TCasScriptMatrixFun.ShortName: string;
begin
  Result := 'matrix';
end;

class function TCasScriptMatrixGet.ShortName: string;
begin
  Result := 'matrix_get';
end;

class function TCasScriptMatrixSet.ShortName: string;
begin
  Result := 'matrix_set';
end;

class function TCasScriptMatrixSet.ArgumentMustBeAssignable(const Index: Integer): boolean;
begin
  Result := Index = 0;
end;

class function TCasScriptMatrixGetCount.ShortName: string;
begin
  Result := 'matrix_get_count';
end;

{ unit init/fini ------------------------------------------------------------- }

initialization
  TCasScriptVec2f.RegisterFunctions;
  TCasScriptVec3f.RegisterFunctions;
  TCasScriptVec4f.RegisterFunctions;

  FunctionHandlers.RegisterHandler(@TCasScriptVec3f(nil).HandleVectorCross, TCasScriptVectorCross, [TCasScriptVec3f, TCasScriptVec3f], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVec3f(nil).HandleGrayscale, TCasScriptGrayscale, [TCasScriptVec3f], false);

  FunctionHandlers.RegisterHandler(@TCasScriptVec4f(nil).HandleOrientationFromDirectionUp, TCasScriptOrientationFromDirectionUp, [TCasScriptVec3f, TCasScriptVec3f], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVec4f(nil).HandleRotate, TCasScriptRotate, [TCasScriptVec4f, TCasScriptVec3f], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVec4f(nil).HandleOrientationToDirection, TCasScriptOrientationToDirection, [TCasScriptVec4f], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVec4f(nil).HandleOrientationToUp, TCasScriptOrientationToUp, [TCasScriptVec4f], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVec4f(nil).HandleSlerp, TCasScriptSlerp, [TCasScriptFloat, TCasScriptVec4f, TCasScriptVec4f], false);

  TCasScriptMatrix3f.RegisterFunctions;
  TCasScriptMatrix4f.RegisterFunctions;

  TCasScriptVec2d.RegisterFunctions;
  TCasScriptVec3d.RegisterFunctions;
  TCasScriptVec4d.RegisterFunctions;

  FunctionHandlers.RegisterHandler(@TCasScriptVec3d(nil).HandleVectorCross, TCasScriptVectorCross, [TCasScriptVec3d, TCasScriptVec3d], false);

  TCasScriptMatrix3d.RegisterFunctions;
  TCasScriptMatrix4d.RegisterFunctions;
end.
