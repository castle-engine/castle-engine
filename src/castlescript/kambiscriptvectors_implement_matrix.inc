{ Common implementation of matrix-based TKamScriptValue descendants.
  Needs following macros defined before including:

  MatrixGetCount
  TKamScriptVecXx
  TKamScriptMatrixXx
  TMatrixXxx
  RegisterMatrixXxFunctions
}

{ TKamScriptMatrixXx ---------------------------------------------------------- }

class procedure TKamScriptMatrixXx.HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptMatrixXx);
  { The function allows only >= 1 arguments, and this handler is
    registered only for TKamScriptMatrixXx values, so we can safely take
    the first arg as TKamScriptMatrixXx. }
  TKamScriptMatrixXx(AResult).Value := TKamScriptMatrixXx(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptMatrixXx(AResult).Value := MatrixAdd(
      TKamScriptMatrixXx(AResult).Value, TKamScriptMatrixXx(Arguments[I]).Value);
end;

class procedure TKamScriptMatrixXx.HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptMatrixXx);
  TKamScriptMatrixXx(AResult).Value := TKamScriptMatrixXx(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptMatrixXx(AResult).Value := MatrixSubtract(
      TKamScriptMatrixXx(AResult).Value, TKamScriptMatrixXx(Arguments[I]).Value);
end;

class procedure TKamScriptMatrixXx.HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptMatrixXx);
  TKamScriptMatrixXx(AResult).Value := MatrixNegate(TKamScriptMatrixXx(Arguments[0]).Value);
end;

class procedure TKamScriptMatrixXx.HandleMultiply(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  if (Length(Arguments) = 2) and
     (Arguments[0] is TKamScriptFloat) and
     (Arguments[1] is TKamScriptMatrixXx) then
  begin
    CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptMatrixXx);
    TKamScriptMatrixXx(AResult).Value := MatrixMultScalar(
      TKamScriptMatrixXx(Arguments[1]).Value,
      TKamScriptFloat(Arguments[0]).Value);
  end else
  if (Length(Arguments) = 2) and
     (Arguments[1] is TKamScriptFloat) and
     (Arguments[0] is TKamScriptMatrixXx) then
  begin
    CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptMatrixXx);
    TKamScriptMatrixXx(AResult).Value := MatrixMultScalar(
      TKamScriptMatrixXx(Arguments[0]).Value,
      TKamScriptFloat(Arguments[1]).Value);
  end else
  if (Length(Arguments) = 2) and
     (Arguments[0] is TKamScriptMatrixXx) and
     (Arguments[1] is TKamScriptVecXx) then
  begin
    CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVecXx);
    TKamScriptVecXx(AResult).Value := MatrixMultVector(
      TKamScriptMatrixXx(Arguments[0]).Value,
      TKamScriptVecXx(Arguments[1]).Value);
  end else
  begin
    { So this is matrix * matrix... operation }
    CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptMatrixXx);
    TKamScriptMatrixXx(AResult).Value := TKamScriptMatrixXx(Arguments[0]).Value;
    for I := 1 to Length(Arguments) - 1 do
      TKamScriptMatrixXx(AResult).Value := MatrixMult(
        TKamScriptMatrixXx(AResult).Value, TKamScriptMatrixXx(Arguments[I]).Value);
  end;
end;

class procedure TKamScriptMatrixXx.HandleDivide(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptMatrixXx);
  TKamScriptMatrixXx(AResult).Value := MatrixMultScalar(
    TKamScriptMatrixXx(Arguments[0]).Value,
    1/TKamScriptFloat(Arguments[1]).Value);
end;

class procedure TKamScriptMatrixXx.HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    MatricesPerfectlyEqual(
      TKamScriptMatrixXx(Arguments[0]).Value,
      TKamScriptMatrixXx(Arguments[1]).Value);
end;

class procedure TKamScriptMatrixXx.HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    not MatricesPerfectlyEqual(
      TKamScriptMatrixXx(Arguments[0]).Value,
      TKamScriptMatrixXx(Arguments[1]).Value);
end;

class procedure TKamScriptMatrixXx.HandleMatrix(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptMatrixXx);
  for I := 0 to MatrixGetCount - 1 do
    TKamScriptMatrixXx(AResult).FValue[I] := TKamScriptVecXx(Arguments[I]).Value;
  TKamScriptMatrixXx(AResult).ValueAssigned := true;
end;

class procedure TKamScriptMatrixXx.HandleMatrixGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVecXx);

  Index := TKamScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, MatrixGetCount - 1) then
    raise EKamScriptError.CreateFmt('Invalid index %d for matrix_get on %d-column matrix',
      [Index, MatrixGetCount]);

  TKamScriptVecXx(AResult).Value := TKamScriptMatrixXx(Arguments[0]).Value[Index];
end;

class procedure TKamScriptMatrixXx.HandleMatrixSet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  Index := TKamScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, MatrixGetCount - 1) then
    raise EKamScriptError.CreateFmt('Invalid index %d for matrix_set on %d-column matrix',
      [Index, MatrixGetCount]);

  TKamScriptMatrixXx(Arguments[0]).FValue[Index] := TKamScriptVecXx(Arguments[2]).Value;
  TKamScriptMatrixXx(Arguments[0]).ValueAssigned := true;

  AResult := Arguments[0];
end;

class procedure TKamScriptMatrixXx.HandleMatrixGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := MatrixGetCount;
end;

procedure TKamScriptMatrixXx.AssignValue(Source: TKamScriptValue);
begin
  if Source is TKamScriptMatrixXx then
    Value := TKamScriptMatrixXx(Source).Value else
    raise EKamAssignValueError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

procedure TKamScriptMatrixXx.SetValue(const AValue: TMatrixXxx);
begin
  FValue := AValue;
  ValueAssigned := true;
end;

procedure RegisterMatrixXxFunctions;
var
  MatrixArgClasses: array of TKamScriptValueClass;
  I: Integer;
begin
  { TKamScriptMatrixXx, functions from KambiScriptCoreFunctions }
  FunctionHandlers.RegisterHandler(@TKamScriptMatrixXx(nil).HandleAdd, TKamScriptAdd, [TKamScriptMatrixXx], true);
  FunctionHandlers.RegisterHandler(@TKamScriptMatrixXx(nil).HandleSubtract, TKamScriptSubtract, [TKamScriptMatrixXx], true);
  FunctionHandlers.RegisterHandler(@TKamScriptMatrixXx(nil).HandleNegate, TKamScriptNegate, [TKamScriptMatrixXx], false);

  FunctionHandlers.RegisterHandler(@TKamScriptMatrixXx(nil).HandleMultiply, TKamScriptMultiply, [TKamScriptMatrixXx, TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptMatrixXx(nil).HandleMultiply, TKamScriptMultiply, [TKamScriptFloat, TKamScriptMatrixXx], false);
  FunctionHandlers.RegisterHandler(@TKamScriptMatrixXx(nil).HandleMultiply, TKamScriptMultiply, [TKamScriptMatrixXx, TKamScriptVecXx], false);
  FunctionHandlers.RegisterHandler(@TKamScriptMatrixXx(nil).HandleMultiply, TKamScriptMultiply, [TKamScriptMatrixXx], true);
  FunctionHandlers.RegisterHandler(@TKamScriptMatrixXx(nil).HandleDivide, TKamScriptDivide, [TKamScriptMatrixXx, TKamScriptFloat], false);

  FunctionHandlers.RegisterHandler(@TKamScriptMatrixXx(nil).HandleEqual, TKamScriptEqual, [TKamScriptMatrixXx, TKamScriptMatrixXx], false);
  FunctionHandlers.RegisterHandler(@TKamScriptMatrixXx(nil).HandleNotEqual, TKamScriptNotEqual, [TKamScriptMatrixXx, TKamScriptMatrixXx], false);

  { TKamScriptMatrixXx, functions from KambiScriptVectors }
  SetLength(MatrixArgClasses, MatrixGetCount);
  for I := 0 to MatrixGetCount - 1 do
    MatrixArgClasses[I] := TKamScriptVecXx;
  FunctionHandlers.RegisterHandler(@TKamScriptMatrixXx(nil).HandleMatrix, TKamScriptMatrixFun, MatrixArgClasses, false);

  FunctionHandlers.RegisterHandler(@TKamScriptMatrixXx(nil).HandleMatrixGet, TKamScriptMatrixGet, [TKamScriptMatrixXx, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptMatrixXx(nil).HandleMatrixSet, TKamScriptMatrixSet, [TKamScriptMatrixXx, TKamScriptInteger, TKamScriptVecXx], false);
  FunctionHandlers.RegisterHandler(@TKamScriptMatrixXx(nil).HandleMatrixGetCount, TKamScriptMatrixGetCount, [TKamScriptMatrixXx], false);
end;
