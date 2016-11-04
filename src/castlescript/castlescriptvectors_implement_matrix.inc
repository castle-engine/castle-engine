{ Common implementation of matrix-based TCasScriptValue descendants.
  Needs following macros defined before including:

  MatrixGetCount
  TCasScriptVecXx
  TCasScriptMatrixXx
  TMatrixXxx
  RegisterMatrixXxFunctions
}

{ TCasScriptMatrixXx ---------------------------------------------------------- }

class procedure TCasScriptMatrixXx.HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptMatrixXx);
  { The function allows only >= 1 arguments, and this handler is
    registered only for TCasScriptMatrixXx values, so we can safely take
    the first arg as TCasScriptMatrixXx. }
  TCasScriptMatrixXx(AResult).Value := TCasScriptMatrixXx(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TCasScriptMatrixXx(AResult).Value := MatrixAdd(
      TCasScriptMatrixXx(AResult).Value, TCasScriptMatrixXx(Arguments[I]).Value);
end;

class procedure TCasScriptMatrixXx.HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptMatrixXx);
  TCasScriptMatrixXx(AResult).Value := TCasScriptMatrixXx(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TCasScriptMatrixXx(AResult).Value := MatrixSubtract(
      TCasScriptMatrixXx(AResult).Value, TCasScriptMatrixXx(Arguments[I]).Value);
end;

class procedure TCasScriptMatrixXx.HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptMatrixXx);
  TCasScriptMatrixXx(AResult).Value := MatrixNegate(TCasScriptMatrixXx(Arguments[0]).Value);
end;

class procedure TCasScriptMatrixXx.HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  if (Length(Arguments) = 2) and
     (Arguments[0] is TCasScriptFloat) and
     (Arguments[1] is TCasScriptMatrixXx) then
  begin
    CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptMatrixXx);
    TCasScriptMatrixXx(AResult).Value := MatrixMultScalar(
      TCasScriptMatrixXx(Arguments[1]).Value,
      TCasScriptFloat(Arguments[0]).Value);
  end else
  if (Length(Arguments) = 2) and
     (Arguments[1] is TCasScriptFloat) and
     (Arguments[0] is TCasScriptMatrixXx) then
  begin
    CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptMatrixXx);
    TCasScriptMatrixXx(AResult).Value := MatrixMultScalar(
      TCasScriptMatrixXx(Arguments[0]).Value,
      TCasScriptFloat(Arguments[1]).Value);
  end else
  if (Length(Arguments) = 2) and
     (Arguments[0] is TCasScriptMatrixXx) and
     (Arguments[1] is TCasScriptVecXx) then
  begin
    CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVecXx);
    TCasScriptVecXx(AResult).Value := MatrixMultVector(
      TCasScriptMatrixXx(Arguments[0]).Value,
      TCasScriptVecXx(Arguments[1]).Value);
  end else
  begin
    { So this is matrix * matrix... operation }
    CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptMatrixXx);
    TCasScriptMatrixXx(AResult).Value := TCasScriptMatrixXx(Arguments[0]).Value;
    for I := 1 to Length(Arguments) - 1 do
      TCasScriptMatrixXx(AResult).Value := MatrixMult(
        TCasScriptMatrixXx(AResult).Value, TCasScriptMatrixXx(Arguments[I]).Value);
  end;
end;

class procedure TCasScriptMatrixXx.HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptMatrixXx);
  TCasScriptMatrixXx(AResult).Value := MatrixMultScalar(
    TCasScriptMatrixXx(Arguments[0]).Value,
    1/TCasScriptFloat(Arguments[1]).Value);
end;

class procedure TCasScriptMatrixXx.HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptMatrixXx);
  TCasScriptMatrixXx(AResult).Value := Lerp(
    TCasScriptFloat(Arguments[0]).Value,
    TCasScriptMatrixXx(Arguments[1]).Value,
    TCasScriptMatrixXx(Arguments[2]).Value);
end;

class procedure TCasScriptMatrixXx.HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptBoolean);
  TCasScriptBoolean(AResult).Value :=
    MatricesPerfectlyEqual(
      TCasScriptMatrixXx(Arguments[0]).Value,
      TCasScriptMatrixXx(Arguments[1]).Value);
end;

class procedure TCasScriptMatrixXx.HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptBoolean);
  TCasScriptBoolean(AResult).Value :=
    not MatricesPerfectlyEqual(
      TCasScriptMatrixXx(Arguments[0]).Value,
      TCasScriptMatrixXx(Arguments[1]).Value);
end;

class procedure TCasScriptMatrixXx.HandleMatrix(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptMatrixXx);
  for I := 0 to MatrixGetCount - 1 do
    TCasScriptMatrixXx(AResult).FValue[I] := TCasScriptVecXx(Arguments[I]).Value;
  TCasScriptMatrixXx(AResult).ValueAssigned := true;
end;

class procedure TCasScriptMatrixXx.HandleMatrixGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVecXx);

  Index := TCasScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, MatrixGetCount - 1) then
    raise ECasScriptError.CreateFmt('Invalid index %d for matrix_get on %d-column matrix',
      [Index, MatrixGetCount]);

  TCasScriptVecXx(AResult).Value := TCasScriptMatrixXx(Arguments[0]).Value[Index];
end;

class procedure TCasScriptMatrixXx.HandleMatrixSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
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

  TCasScriptMatrixXx(Arguments[0]).FValue[Index] := TCasScriptVecXx(Arguments[2]).Value;
  TCasScriptMatrixXx(Arguments[0]).ValueAssigned := true;

  AResult := Arguments[0];
end;

class procedure TCasScriptMatrixXx.HandleMatrixGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptInteger);
  TCasScriptInteger(AResult).Value := MatrixGetCount;
end;

procedure TCasScriptMatrixXx.AssignValue(Source: TCasScriptValue);
begin
  if Source is TCasScriptMatrixXx then
    Value := TCasScriptMatrixXx(Source).Value else
    raise ECasScriptAssignError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

procedure TCasScriptMatrixXx.SetValue(const AValue: TMatrixXxx);
begin
  FValue := AValue;
  ValueAssigned := true;
end;

procedure RegisterMatrixXxFunctions;
var
  MatrixArgClasses: array of TCasScriptValueClass;
  I: Integer;
begin
  { TCasScriptMatrixXx, functions from CastleScriptCoreFunctions }
  FunctionHandlers.RegisterHandler(@TCasScriptMatrixXx(nil).HandleAdd, TCasScriptAdd, [TCasScriptMatrixXx], true);
  FunctionHandlers.RegisterHandler(@TCasScriptMatrixXx(nil).HandleSubtract, TCasScriptSubtract, [TCasScriptMatrixXx], true);
  FunctionHandlers.RegisterHandler(@TCasScriptMatrixXx(nil).HandleNegate, TCasScriptNegate, [TCasScriptMatrixXx], false);

  FunctionHandlers.RegisterHandler(@TCasScriptMatrixXx(nil).HandleMultiply, TCasScriptMultiply, [TCasScriptMatrixXx, TCasScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TCasScriptMatrixXx(nil).HandleMultiply, TCasScriptMultiply, [TCasScriptFloat, TCasScriptMatrixXx], false);
  FunctionHandlers.RegisterHandler(@TCasScriptMatrixXx(nil).HandleMultiply, TCasScriptMultiply, [TCasScriptMatrixXx, TCasScriptVecXx], false);
  FunctionHandlers.RegisterHandler(@TCasScriptMatrixXx(nil).HandleMultiply, TCasScriptMultiply, [TCasScriptMatrixXx], true);
  FunctionHandlers.RegisterHandler(@TCasScriptMatrixXx(nil).HandleDivide, TCasScriptDivide, [TCasScriptMatrixXx, TCasScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TCasScriptMatrixXx(nil).HandleLerp, TCasScriptLerp, [TCasScriptFloat, TCasScriptMatrixXx, TCasScriptMatrixXx], false);

  FunctionHandlers.RegisterHandler(@TCasScriptMatrixXx(nil).HandleEqual, TCasScriptEqual, [TCasScriptMatrixXx, TCasScriptMatrixXx], false);
  FunctionHandlers.RegisterHandler(@TCasScriptMatrixXx(nil).HandleNotEqual, TCasScriptNotEqual, [TCasScriptMatrixXx, TCasScriptMatrixXx], false);

  { TCasScriptMatrixXx, functions from CastleScriptVectors }
  SetLength(MatrixArgClasses, MatrixGetCount);
  for I := 0 to MatrixGetCount - 1 do
    MatrixArgClasses[I] := TCasScriptVecXx;
  FunctionHandlers.RegisterHandler(@TCasScriptMatrixXx(nil).HandleMatrix, TCasScriptMatrixFun, MatrixArgClasses, false);

  FunctionHandlers.RegisterHandler(@TCasScriptMatrixXx(nil).HandleMatrixGet, TCasScriptMatrixGet, [TCasScriptMatrixXx, TCasScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TCasScriptMatrixXx(nil).HandleMatrixSet, TCasScriptMatrixSet, [TCasScriptMatrixXx, TCasScriptInteger, TCasScriptVecXx], false);
  FunctionHandlers.RegisterHandler(@TCasScriptMatrixXx(nil).HandleMatrixGetCount, TCasScriptMatrixGetCount, [TCasScriptMatrixXx], false);
end;
