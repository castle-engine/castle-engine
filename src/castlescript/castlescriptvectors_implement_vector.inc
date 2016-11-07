{ Common implementation of vector-based TCasScriptValue descendants.
  Needs following macros defined before including:

  VectorGetCount
  TCasScriptVectXx
  TVectorXxx
  RegisterVecXxFunctions
  TCasScriptVectorFunXxx
}

{ TCasScriptVecXx ---------------------------------------------------------- }

class procedure TCasScriptVecXx.HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVecXx);
  { The function allows only >= 1 arguments, and this handler is
    registered only for TCasScriptVecXx values, so we can safely take
    the first arg as TCasScriptVecXx. }
  TCasScriptVecXx(AResult).Value := TCasScriptVecXx(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TCasScriptVecXx(AResult).Value := VectorAdd(
      TCasScriptVecXx(AResult).Value, TCasScriptVecXx(Arguments[I]).Value);
end;

class procedure TCasScriptVecXx.HandleSubtract(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVecXx);
  TCasScriptVecXx(AResult).Value := TCasScriptVecXx(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TCasScriptVecXx(AResult).Value := VectorSubtract(
      TCasScriptVecXx(AResult).Value, TCasScriptVecXx(Arguments[I]).Value);
end;

class procedure TCasScriptVecXx.HandleNegate(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVecXx);
  TCasScriptVecXx(AResult).Value := VectorNegate(TCasScriptVecXx(Arguments[0]).Value);
end;

class procedure TCasScriptVecXx.HandleEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptBoolean);
  TCasScriptBoolean(AResult).Value :=
    VectorsPerfectlyEqual(
      TCasScriptVecXx(Arguments[0]).Value,
      TCasScriptVecXx(Arguments[1]).Value);
end;

class procedure TCasScriptVecXx.HandleNotEqual(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptBoolean);
  TCasScriptBoolean(AResult).Value :=
    not VectorsPerfectlyEqual(
      TCasScriptVecXx(Arguments[0]).Value,
      TCasScriptVecXx(Arguments[1]).Value);
end;

class procedure TCasScriptVecXx.HandleMultiply(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVecXx);
  if Arguments[0] is TCasScriptFloat then
  begin
    TCasScriptVecXx(AResult).Value := VectorScale(
      TCasScriptVecXx(Arguments[1]).Value,
      TCasScriptFloat(Arguments[0]).Value);
  end else
  begin
    TCasScriptVecXx(AResult).Value := VectorScale(
      TCasScriptVecXx(Arguments[0]).Value,
      TCasScriptFloat(Arguments[1]).Value);
  end;
end;

class procedure TCasScriptVecXx.HandleDivide(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVecXx);
  TCasScriptVecXx(AResult).Value := VectorScale(
    TCasScriptVecXx(Arguments[0]).Value,
    1/TCasScriptFloat(Arguments[1]).Value);
end;

class procedure TCasScriptVecXx.HandleLerp(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVecXx);
  TCasScriptVecXx(AResult).Value := Lerp(
    TCasScriptFloat(Arguments[0]).Value,
    TCasScriptVecXx(Arguments[1]).Value,
    TCasScriptVecXx(Arguments[2]).Value);
end;

class procedure TCasScriptVecXx.HandleMax(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVecXx);
  for I := 0 to VectorGetCount - 1 do
    TCasScriptVecXx(AResult).FValue[I] :=
      Max( TCasScriptVecXx(Arguments[0]).FValue[I],
           TCasScriptVecXx(Arguments[1]).FValue[I] );
  TCasScriptVecXx(AResult).ValueAssigned := true;
end;

class procedure TCasScriptVecXx.HandleMin(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVecXx);
  for I := 0 to VectorGetCount - 1 do
    TCasScriptVecXx(AResult).FValue[I] :=
      Min( TCasScriptVecXx(Arguments[0]).FValue[I],
           TCasScriptVecXx(Arguments[1]).FValue[I] );
  TCasScriptVecXx(AResult).ValueAssigned := true;
end;

class procedure TCasScriptVecXx.HandleVector(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptVecXx);
  for I := 0 to VectorGetCount - 1 do
    TCasScriptVecXx(AResult).FValue[I] := TCasScriptFloat(Arguments[I]).Value;
  TCasScriptVecXx(AResult).ValueAssigned := true;
end;

class procedure TCasScriptVecXx.HandleVectorGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);

  Index := TCasScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, VectorGetCount - 1) then
    raise ECasScriptError.CreateFmt('Invalid index %d for vector_get on %d-element vector',
      [Index, VectorGetCount]);

  TCasScriptFloat(AResult).Value := TCasScriptVecXx(Arguments[0]).Value[Index];
end;

class procedure TCasScriptVecXx.HandleVectorSet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
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

  TCasScriptVecXx(Arguments[0]).FValue[Index] := TCasScriptFloat(Arguments[2]).Value;
  TCasScriptVecXx(Arguments[0]).ValueAssigned := true;

  AResult := Arguments[0];
end;

class procedure TCasScriptVecXx.HandleVectorGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptInteger);
  TCasScriptInteger(AResult).Value := VectorGetCount;
end;

class procedure TCasScriptVecXx.HandleVectorLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
  TCasScriptFloat(AResult).Value :=
    VectorLen(TCasScriptVecXx(Arguments[0]).Value);
end;

class procedure TCasScriptVecXx.HandleVectorSqrLength(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
  TCasScriptFloat(AResult).Value :=
    VectorLenSqr(TCasScriptVecXx(Arguments[0]).Value);
end;

class procedure TCasScriptVecXx.HandleVectorDot(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptFloat);
  TCasScriptFloat(AResult).Value :=
    VectorDotProduct( TCasScriptVecXx(Arguments[0]).Value,
                      TCasScriptVecXx(Arguments[1]).Value );
end;

procedure TCasScriptVecXx.AssignValue(Source: TCasScriptValue);
begin
  if Source is TCasScriptVecXx then
    Value := TCasScriptVecXx(Source).Value else
    raise ECasScriptAssignError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

procedure TCasScriptVecXx.SetValue(const AValue: TVectorXxx);
begin
  FValue := AValue;
  ValueAssigned := true;
end;

procedure RegisterVecXxFunctions;
var
  VectorArgClasses: array of TCasScriptValueClass;
  I: Integer;
begin
  { TCasScriptVecXx, functions from CastleScriptCoreFunctions }
  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleAdd, TCasScriptAdd, [TCasScriptVecXx], true);
  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleSubtract, TCasScriptSubtract, [TCasScriptVecXx], true);
  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleNegate, TCasScriptNegate, [TCasScriptVecXx], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleEqual, TCasScriptEqual, [TCasScriptVecXx, TCasScriptVecXx], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleNotEqual, TCasScriptNotEqual, [TCasScriptVecXx, TCasScriptVecXx], false);

  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleMultiply, TCasScriptMultiply, [TCasScriptVecXx, TCasScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleMultiply, TCasScriptMultiply, [TCasScriptFloat, TCasScriptVecXx], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleDivide, TCasScriptDivide, [TCasScriptVecXx, TCasScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleLerp, TCasScriptLerp, [TCasScriptFloat, TCasScriptVecXx, TCasScriptVecXx], false);

  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleMax, TCasScriptMax, [TCasScriptVecXx, TCasScriptVecXx], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleMin, TCasScriptMin, [TCasScriptVecXx, TCasScriptVecXx], false);

  { TCasScriptVecXx, functions from CastleScriptVectors }
  SetLength(VectorArgClasses, VectorGetCount);
  for I := 0 to VectorGetCount - 1 do
    VectorArgClasses[I] := TCasScriptFloat;
  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleVector, TCasScriptVectorFunXxx, VectorArgClasses, false);

  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleVectorGet, TCasScriptVectorGet, [TCasScriptVecXx, TCasScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleVectorSet, TCasScriptVectorSet, [TCasScriptVecXx, TCasScriptInteger, TCasScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleVectorGetCount, TCasScriptVectorGetCount, [TCasScriptVecXx], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleVectorLength, TCasScriptVectorLength, [TCasScriptVecXx], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleVectorSqrLength, TCasScriptVectorSqrLength, [TCasScriptVecXx], false);
  FunctionHandlers.RegisterHandler(@TCasScriptVecXx(nil).HandleVectorDot, TCasScriptVectorDot, [TCasScriptVecXx, TCasScriptVecXx], false);
end;
