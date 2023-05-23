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

{ CastleScript vector and matrix types and built-in functions. }
unit CastleScriptVectors;

{$I castleconf.inc}

interface

uses CastleVectors, CastleScript;

{$ifdef CASTLE_SCRIPT_FPC} // TODO: Our current usage of generics doesn't compile with Delphi

type
  TCasScriptVector = class;
  TCasScriptVectorD = class;

  {$ifdef FPC}generic{$endif}
  TCasScriptVec<
    TVectorXxx,
    TCasScriptVectorFunXxx> = class(TCasScriptValue)
  private
    type
      TSelfClass = TCasScriptVec {$ifndef FPC} <
        TVectorXxx,
        TCasScriptVectorFunXxx> {$endif};

    { Create and make Value an instance of TSelfClass. }
    class function CreateValueIfNeededSelf(var Value: TCasScriptValue;
      var ParentOfValue: boolean): TSelfClass;

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

  TCasScriptVec2f = class({$ifdef FPC}specialize{$endif} TCasScriptVec<
    TVector2,
    TCasScriptVector>)
  end;

  TCasScriptVec3f = class({$ifdef FPC}specialize{$endif} TCasScriptVec<
    TVector3,
    TCasScriptVector>)
  private
    class procedure HandleVectorCross(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
    class procedure HandleGrayscale(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  end;

  TCasScriptVec4f = class({$ifdef FPC}specialize{$endif} TCasScriptVec<
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

  TCasScriptVec2d = class({$ifdef FPC}specialize{$endif} TCasScriptVec<
    TVector2Double,
    TCasScriptVectorD>)
  end;

  TCasScriptVec3d = class({$ifdef FPC}specialize{$endif} TCasScriptVec<
    TVector3Double,
    TCasScriptVectorD>)
  private
    class procedure HandleVectorCross(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
  end;

  TCasScriptVec4d = class({$ifdef FPC}specialize{$endif} TCasScriptVec<
    TVector4Double,
    TCasScriptVectorD>)
  end;

  {$ifdef FPC}generic{$endif}
  TCasScriptMatrix<
    TCasScriptVecXxx,
    TMatrixXxx,
    TVectorXxx> = class(TCasScriptValue)
  private
    type
      TSelfClass = TCasScriptMatrix {$ifndef FPC} <
        TCasScriptVecXxx,
        TMatrixXxx,
        TVectorXxx> {$endif};

    { Create and make Value an instance of TSelfClass. }
    class function CreateValueIfNeededSelf(var Value: TCasScriptValue;
      var ParentOfValue: boolean): TSelfClass;

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

  TCasScriptMatrix3f = class({$ifdef FPC}specialize{$endif} TCasScriptMatrix<
    TCasScriptVec3f,
    TMatrix3,
    TVector3>)
  end;

  TCasScriptMatrix4f = class({$ifdef FPC}specialize{$endif} TCasScriptMatrix<
    TCasScriptVec4f,
    TMatrix4,
    TVector4>)
  end;

  TCasScriptMatrix3d = class({$ifdef FPC}specialize{$endif} TCasScriptMatrix<
    TCasScriptVec3d,
    TMatrix3Double,
    TVector3Double>)
  end;

  TCasScriptMatrix4d = class({$ifdef FPC}specialize{$endif} TCasScriptMatrix<
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

{$endif CASTLE_SCRIPT_FPC}

implementation

uses Math,
  CastleScriptCoreFunctions, CastleUtils, CastleLog, CastleTransform,
  CastleQuaternions, CastleColors;

{$ifdef CASTLE_SCRIPT_FPC} // TODO: Our current usage of generics doesn't compile with Delphi

{ TCasScriptVec ---------------------------------------------------------- }

class function TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  CreateValueIfNeededSelf(var Value: TCasScriptValue; var ParentOfValue: boolean): TSelfClass;
begin
  CreateValueIfNeeded(Value, ParentOfValue,
    {$ifdef FPC}TCasScriptValueClass(ClassType){$else}Self{$endif});
  Result := TSelfClass(Value);
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
  MyResult: TSelfClass;
  {$ifndef FPC}
  Arg: TSelfClass;
  {$endif}
begin
  MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);

  { The function allows only >= 1 arguments, and this handler is
    registered only for TCasScriptVec values, so we can safely take
    the first arg as TCasScriptVec. }
  MyResult.Value := TSelfClass(Arguments[0]).Value;

  for I := 1 to Length(Arguments) - 1 do
  begin
    {$ifndef FPC}Arg := (Arguments[I] as TSelfClass);{$endif}
    MyResult.Value := MyResult.Value + {$ifdef FPC}TSelfClass(Arguments[I]){$else}Arg{$endif}.Value;
  end;
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
  MyResult: TSelfClass;
begin
  MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
  MyResult.Value := TSelfClass(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    MyResult.Value := MyResult.Value - TSelfClass(Arguments[I]).Value;
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  MyResult: TSelfClass;
begin
  MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
  MyResult.Value := -TSelfClass(Arguments[0]).Value;
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptBoolean);
  TCasScriptBoolean(AResult).Value :=
    TVectorXxx.PerfectlyEquals(
      TSelfClass(Arguments[0]).Value,
      TSelfClass(Arguments[1]).Value);
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptBoolean);
  TCasScriptBoolean(AResult).Value :=
    not TVectorXxx.PerfectlyEquals(
      TSelfClass(Arguments[0]).Value,
      TSelfClass(Arguments[1]).Value);
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  MyResult: TSelfClass;
begin
  MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
  if Arguments[0] is TCasScriptFloat then
  begin
    MyResult.Value :=
      TSelfClass(Arguments[1]).Value *
      TCasScriptFloat(Arguments[0]).Value;
  end else
  begin
    MyResult.Value :=
      TSelfClass(Arguments[0]).Value *
      TCasScriptFloat(Arguments[1]).Value;
  end;
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  MyResult: TSelfClass;
begin
  MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
  MyResult.Value :=
    TSelfClass(Arguments[0]).Value *
    1/TCasScriptFloat(Arguments[1]).Value;
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  MyResult: TSelfClass;
begin
  MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
  MyResult.Value := TVectorXxx.Lerp(
    TCasScriptFloat(Arguments[0]).Value,
    TSelfClass(Arguments[1]).Value,
    TSelfClass(Arguments[2]).Value);
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleMax(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
  MyResult: TSelfClass;
begin
  MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
  for I := 0 to VectorGetCount - 1 do
    MyResult.FValue.Data[I] :=
      Max( TSelfClass(Arguments[0]).FValue[I],
           TSelfClass(Arguments[1]).FValue[I] );
  MyResult.ValueAssigned := true;
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleMin(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
  MyResult: TSelfClass;
begin
  MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
  for I := 0 to VectorGetCount - 1 do
    MyResult.FValue.Data[I] :=
      Min( TSelfClass(Arguments[0]).FValue[I],
           TSelfClass(Arguments[1]).FValue[I] );
  MyResult.ValueAssigned := true;
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleVector(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
  MyResult: TSelfClass;
begin
  MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
  for I := 0 to VectorGetCount - 1 do
    MyResult.FValue.Data[I] := TCasScriptFloat(Arguments[I]).Value;
  MyResult.ValueAssigned := true;
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleVectorGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);

  Index := TCasScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, VectorGetCount - 1) then
    raise ECasScriptError.CreateFmt('Invalid index %d for vector_get on %d-element vector',
      [Index, VectorGetCount]);

  TCasScriptFloat(AResult).Value := TSelfClass(Arguments[0]).Value[Index];
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleVectorSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
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

  TSelfClass(Arguments[0]).FValue.Data[Index] := TCasScriptFloat(Arguments[2]).Value;
  TSelfClass(Arguments[0]).ValueAssigned := true;

  AResult := Arguments[0];
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleVectorGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptInteger);
  TCasScriptInteger(AResult).Value := VectorGetCount;
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleVectorLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
  TCasScriptFloat(AResult).Value := TSelfClass(Arguments[0]).Value.Length;
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleVectorSqrLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
  TCasScriptFloat(AResult).Value := TSelfClass(Arguments[0]).Value.LengthSqr;
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  HandleVectorDot(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
  TCasScriptFloat(AResult).Value :=
    TVectorXxx.DotProduct(
      TSelfClass(Arguments[0]).Value,
      TSelfClass(Arguments[1]).Value );
end;

procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  AssignValue(Source: TCasScriptValue);
begin
  if Source is TSelfClass then
    Value := TSelfClass(Source).Value
  else
    raise ECasScriptAssignError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  SetValue(const AValue: TVectorXxx);
begin
  FValue := AValue;
  ValueAssigned := true;
end;

class function TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  VectorGetCount: Integer;
begin
  Result := High(TVectorXxx.TIndex) + 1;
end;

class procedure TCasScriptVec {$ifndef FPC} <
  TVectorXxx,
  TCasScriptVectorFunXxx> {$endif} .
  RegisterFunctions;
var
  VectorArgClasses: array of TCasScriptValueClass;
  I: Integer;
begin
  { functions from CastleScriptCoreFunctions }
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleAdd, TCasScriptAdd, [TSelfClass], true);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleSubtract, TCasScriptSubtract, [TSelfClass], true);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleNegate, TCasScriptNegate, [TSelfClass], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleEqual, TCasScriptEqual, [TSelfClass, TSelfClass], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleNotEqual, TCasScriptNotEqual, [TSelfClass, TSelfClass], false);

  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleMultiply, TCasScriptMultiply, [TSelfClass, TCasScriptFloat], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleMultiply, TCasScriptMultiply, [TCasScriptFloat, TSelfClass], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleDivide, TCasScriptFloatDivide, [TSelfClass, TCasScriptFloat], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleLerp, TCasScriptLerp, [TCasScriptFloat, TSelfClass, TSelfClass], false);

  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleMax, TCasScriptMax, [TSelfClass, TSelfClass], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleMin, TCasScriptMin, [TSelfClass, TSelfClass], false);

  { functions from CastleScriptVectors }
  SetLength(VectorArgClasses, VectorGetCount);
  for I := 0 to VectorGetCount - 1 do
    VectorArgClasses[I] := TCasScriptFloat;
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleVector, TCasScriptVectorFunXxx, VectorArgClasses, false);

  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleVectorGet, TCasScriptVectorGet, [TSelfClass, TCasScriptInteger], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleVectorSet, TCasScriptVectorSet, [TSelfClass, TCasScriptInteger, TCasScriptFloat], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleVectorGetCount, TCasScriptVectorGetCount, [TSelfClass], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleVectorLength, TCasScriptVectorLength, [TSelfClass], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleVectorSqrLength, TCasScriptVectorSqrLength, [TSelfClass], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleVectorDot, TCasScriptVectorDot, [TSelfClass, TSelfClass], false);
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
  { no need to normalize Dir, Up here (OrientationFromDirectionUp will do it) }

  TCasScriptVec4f(AResult).Value := OrientationFromDirectionUp(Dir, Up);
end;

class procedure TCasScriptVec4f.HandleRotateCore(
  const Rotation: TVector4; const Point: TVector3;
  var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Axis: TVector3 absolute Rotation;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVec3f);

  if not Axis.IsZero then
    TCasScriptVec3f(AResult).Value := RotatePointAroundAxis(Rotation, Point)
  else
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

class function TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  CreateValueIfNeededSelf(var Value: TCasScriptValue; var ParentOfValue: boolean): TSelfClass;
begin
  CreateValueIfNeeded(Value, ParentOfValue, TCasScriptValueClass(ClassType));
  Result := TSelfClass(Value);
end;

class procedure TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
  MyResult: TSelfClass;
begin
  MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
  { The function allows only >= 1 arguments, and this handler is
    registered only for TSelfClass values, so we can safely take
    the first arg as TSelfClass. }
  MyResult.Value := TSelfClass(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    MyResult.Value := MyResult.Value + TSelfClass(Arguments[I]).Value;
end;

class procedure TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
  MyResult: TSelfClass;
begin
  MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
  MyResult.Value := TSelfClass(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    MyResult.Value :=  MyResult.Value - TSelfClass(Arguments[I]).Value;
end;

class procedure TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  MyResult: TSelfClass;
begin
  MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
  MyResult.Value := -TSelfClass(Arguments[0]).Value;
end;

class procedure TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
var
  MyResult: TSelfClass;
begin
  if (Length(Arguments) = 2) and
     (Arguments[0] is TCasScriptFloat) and
     (Arguments[1] is TSelfClass) then
  begin
    MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
    MyResult.Value :=
      TSelfClass(Arguments[1]).Value *
      TCasScriptFloat(Arguments[0]).Value;
  end else
  if (Length(Arguments) = 2) and
     (Arguments[1] is TCasScriptFloat) and
     (Arguments[0] is TSelfClass) then
  begin
    MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
    MyResult.Value :=
      TSelfClass(Arguments[0]).Value *
      TCasScriptFloat(Arguments[1]).Value;
  end else
  if (Length(Arguments) = 2) and
     (Arguments[0] is TSelfClass) and
     (Arguments[1].InheritsFrom(TCasScriptVecXxx)) then
  begin
    CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVecXxx);
    TCasScriptVecXxx(AResult).Value :=
      TSelfClass(Arguments[0]).Value *
      TCasScriptVecXxx(Arguments[1]).Value;
  end else
  begin
    { So this is matrix * matrix... operation }
    MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
    MyResult.Value := TSelfClass(Arguments[0]).Value;
    for I := 1 to Length(Arguments) - 1 do
      MyResult.Value :=
        MyResult.Value *
        TSelfClass(Arguments[I]).Value;
  end;
end;

class procedure TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  MyResult: TSelfClass;
begin
  MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
  MyResult.Value :=
    TSelfClass(Arguments[0]).Value *
    (1/TCasScriptFloat(Arguments[1]).Value);
end;

class procedure TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  MyResult: TSelfClass;
begin
  MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
  MyResult.Value := TMatrixXxx.Lerp(
    TCasScriptFloat(Arguments[0]).Value,
    TSelfClass(Arguments[1]).Value,
    TSelfClass(Arguments[2]).Value);
end;

class procedure TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptBoolean);
  TCasScriptBoolean(AResult).Value :=
    TMatrixXxx.PerfectlyEquals(
      TSelfClass(Arguments[0]).Value,
      TSelfClass(Arguments[1]).Value);
end;

class procedure TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptBoolean);
  TCasScriptBoolean(AResult).Value :=
    not TMatrixXxx.PerfectlyEquals(
      TSelfClass(Arguments[0]).Value,
      TSelfClass(Arguments[1]).Value);
end;

class procedure TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  HandleMatrix(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
  MyResult: TSelfClass;
begin
  MyResult := CreateValueIfNeededSelf(AResult, ParentOfResult);
  for I := 0 to MatrixGetCount - 1 do
    MyResult.FValue.Columns[I] := TCasScriptVecXxx(Arguments[I]).Value;
  MyResult.ValueAssigned := true;
end;

class procedure TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  HandleMatrixGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
  V: TVectorXxx;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVecXxx);

  Index := TCasScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, MatrixGetCount - 1) then
    raise ECasScriptError.CreateFmt('Invalid index %d for matrix_get on %d-column matrix',
      [Index, MatrixGetCount]);

  V := TSelfClass(Arguments[0]).Value.Columns[Index];
  TCasScriptVecXxx(AResult).Value := V;
end;

class procedure TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  HandleMatrixSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
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

  TSelfClass(Arguments[0]).FValue.Columns[Index] := TCasScriptVecXxx(Arguments[2]).Value;
  TSelfClass(Arguments[0]).ValueAssigned := true;

  AResult := Arguments[0];
end;

class procedure TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  HandleMatrixGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptInteger);
  TCasScriptInteger(AResult).Value := MatrixGetCount;
end;

procedure TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  AssignValue(Source: TCasScriptValue);
begin
  if Source is TSelfClass then
    Value := TSelfClass(Source).Value else
    raise ECasScriptAssignError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

procedure TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  SetValue(const AValue: TMatrixXxx);
begin
  FValue := AValue;
  ValueAssigned := true;
end;

class function TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  MatrixGetCount: Integer;
begin
  Result := High(TVectorXxx.TIndex) + 1;
end;

class procedure TCasScriptMatrix {$ifndef FPC} <TCasScriptVecXxx,
  TMatrixXxx,
  TVectorXxx> {$endif} .
  RegisterFunctions;
var
  MatrixArgClasses: array of TCasScriptValueClass;
  I: Integer;
begin
  { functions from CastleScriptCoreFunctions }
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleAdd, TCasScriptAdd, [TSelfClass], true);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleSubtract, TCasScriptSubtract, [TSelfClass], true);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleNegate, TCasScriptNegate, [TSelfClass], false);

  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleMultiply, TCasScriptMultiply, [TSelfClass, TCasScriptFloat], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleMultiply, TCasScriptMultiply, [TCasScriptFloat, TSelfClass], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleMultiply, TCasScriptMultiply, [TSelfClass, TCasScriptVecXxx], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleMultiply, TCasScriptMultiply, [TSelfClass], true);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleDivide, TCasScriptFloatDivide, [TSelfClass, TCasScriptFloat], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleLerp, TCasScriptLerp, [TCasScriptFloat, TSelfClass, TSelfClass], false);

  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleEqual, TCasScriptEqual, [TSelfClass, TSelfClass], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleNotEqual, TCasScriptNotEqual, [TSelfClass, TSelfClass], false);

  { functions from CastleScriptVectors }
  SetLength(MatrixArgClasses, MatrixGetCount);
  for I := 0 to MatrixGetCount - 1 do
    MatrixArgClasses[I] := TCasScriptVecXxx;
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleMatrix, TCasScriptMatrixFun, MatrixArgClasses, false);

  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleMatrixGet, TCasScriptMatrixGet, [TSelfClass, TCasScriptInteger], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleMatrixSet, TCasScriptMatrixSet, [TSelfClass, TCasScriptInteger, TCasScriptVecXxx], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} HandleMatrixGetCount, TCasScriptMatrixGetCount, [TSelfClass], false);
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

  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptVec3f {$ifdef FPC}(nil){$endif} .HandleVectorCross, TCasScriptVectorCross, [TCasScriptVec3f, TCasScriptVec3f], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptVec3f {$ifdef FPC}(nil){$endif} .HandleGrayscale, TCasScriptGrayscale, [TCasScriptVec3f], false);

  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptVec4f {$ifdef FPC}(nil){$endif} .HandleOrientationFromDirectionUp, TCasScriptOrientationFromDirectionUp, [TCasScriptVec3f, TCasScriptVec3f], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptVec4f {$ifdef FPC}(nil){$endif} .HandleRotate, TCasScriptRotate, [TCasScriptVec4f, TCasScriptVec3f], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptVec4f {$ifdef FPC}(nil){$endif} .HandleOrientationToDirection, TCasScriptOrientationToDirection, [TCasScriptVec4f], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptVec4f {$ifdef FPC}(nil){$endif} .HandleOrientationToUp, TCasScriptOrientationToUp, [TCasScriptVec4f], false);
  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptVec4f {$ifdef FPC}(nil){$endif} .HandleSlerp, TCasScriptSlerp, [TCasScriptFloat, TCasScriptVec4f, TCasScriptVec4f], false);

  TCasScriptMatrix3f.RegisterFunctions;
  TCasScriptMatrix4f.RegisterFunctions;

  TCasScriptVec2d.RegisterFunctions;
  TCasScriptVec3d.RegisterFunctions;
  TCasScriptVec4d.RegisterFunctions;

  FunctionHandlers.RegisterHandler({$ifdef FPC}@{$endif} TCasScriptVec3d {$ifdef FPC}(nil){$endif}. HandleVectorCross, TCasScriptVectorCross, [TCasScriptVec3d, TCasScriptVec3d], false);

  TCasScriptMatrix3d.RegisterFunctions;
  TCasScriptMatrix4d.RegisterFunctions;

{$endif CASTLE_SCRIPT_FPC}
end.
