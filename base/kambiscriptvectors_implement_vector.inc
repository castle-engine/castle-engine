{ Common implementation of vector-based TKamScriptValue descendants.
  Needs following macros defined before including:

  VectorGetCount
  TKamScriptVectXx
  TVectorXxx
}

{ TKamScriptVecXx ---------------------------------------------------------- }

class procedure TKamScriptVecXx.HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVecXx);
  { The function allows only >= 1 arguments, and this handler is
    registered only for TKamScriptVecXx values, so we can safely take
    the first arg as TKamScriptVecXx. }
  TKamScriptVecXx(AResult).Value := TKamScriptVecXx(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptVecXx(AResult).Value := VectorAdd(
      TKamScriptVecXx(AResult).Value, TKamScriptVecXx(Arguments[I]).Value);
end;

class procedure TKamScriptVecXx.HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVecXx);
  TKamScriptVecXx(AResult).Value := TKamScriptVecXx(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptVecXx(AResult).Value := VectorSubtract(
      TKamScriptVecXx(AResult).Value, TKamScriptVecXx(Arguments[I]).Value);
end;

class procedure TKamScriptVecXx.HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVecXx);
  TKamScriptVecXx(AResult).Value := VectorNegate(TKamScriptVecXx(Arguments[0]).Value);
end;

class procedure TKamScriptVecXx.HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    VectorsPerfectlyEqual(
      TKamScriptVecXx(Arguments[0]).Value,
      TKamScriptVecXx(Arguments[1]).Value);
end;

class procedure TKamScriptVecXx.HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    not VectorsPerfectlyEqual(
      TKamScriptVecXx(Arguments[0]).Value,
      TKamScriptVecXx(Arguments[1]).Value);
end;

class procedure TKamScriptVecXx.HandleVector(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVecXx);
  for I := 0 to VectorGetCount - 1 do
    TKamScriptVecXx(AResult).FValue[I] := TKamScriptFloat(Arguments[I]).Value;
  TKamScriptVecXx(AResult).ValueAssigned := true;
end;

class procedure TKamScriptVecXx.HandleVectorGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);

  Index := TKamScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, VectorGetCount - 1) then
    raise EKamScriptError.CreateFmt('Invalid index %d for vector_get on %d-element vector',
      [Index, VectorGetCount]);

  TKamScriptFloat(AResult).Value := TKamScriptVecXx(Arguments[0]).Value[Index];
end;

class procedure TKamScriptVecXx.HandleVectorSet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  Index := TKamScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, VectorGetCount - 1) then
    raise EKamScriptError.CreateFmt('Invalid index %d for vector_set on %d-element vector',
      [Index, VectorGetCount]);

  TKamScriptVecXx(Arguments[0]).FValue[Index] := TKamScriptFloat(Arguments[2]).Value;
  TKamScriptVecXx(Arguments[0]).ValueAssigned := true;

  AResult := Arguments[0];
end;

class procedure TKamScriptVecXx.HandleVectorGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := VectorGetCount;
end;

class procedure TKamScriptVecXx.HandleVectorLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value :=
    VectorLen(TKamScriptVecXx(Arguments[0]).Value);
end;

class procedure TKamScriptVecXx.HandleVectorSqrLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value :=
    VectorLenSqr(TKamScriptVecXx(Arguments[0]).Value);
end;

class procedure TKamScriptVecXx.HandleVectorDot(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value :=
    VectorDotProduct( TKamScriptVecXx(Arguments[0]).Value,
                      TKamScriptVecXx(Arguments[1]).Value );
end;

procedure TKamScriptVecXx.AssignValue(Source: TKamScriptValue);
begin
  if Source is TKamScriptVecXx then
    Value := TKamScriptVecXx(Source).Value else
    raise EKamAssignValueError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

procedure TKamScriptVecXx.SetValue(const AValue: TVector2Single);
begin
  FValue := AValue;
  ValueAssigned := true;
end;
