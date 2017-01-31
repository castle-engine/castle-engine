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

constructor TCasScriptXxxArray.Create(const AWriteable: boolean; const AValue: TXxxList);
begin
  Create(AWriteable);
  Value := AValue;
end;

constructor TCasScriptXxxArray.Create(const AWriteable: boolean);
begin
  inherited;
  FValue := TXxxList.Create;
end;

destructor TCasScriptXxxArray.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;

procedure TCasScriptXxxArray.SetValue(const AValue: TXxxList);
begin
  FValue.Assign(AValue);
  ValueAssigned := true;
end;

procedure TCasScriptXxxArray.AssignValue(Source: TCasScriptValue);
begin
  if Source is TCasScriptXxxArray then
    Value := TCasScriptXxxArray(Source).Value else
    raise ECasScriptAssignError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

class procedure TCasScriptXxxArray.HandleArrayFun(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
  Arr: TXxxList;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptXxxArray);

  Arr := TCasScriptXxxArray(AResult).Value;
  Arr.Count := Length(Arguments);
  for I := 0 to Length(Arguments) - 1 do
    Arr.L[I] := TCasScriptXxxElement(Arguments[I]).Value;

  TCasScriptXxxArray(AResult).ValueAssigned := true;
end;

class procedure TCasScriptXxxArray.HandleArrayGetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptInteger);
  TCasScriptInteger(AResult).Value :=
    TCasScriptXxxArray(Arguments[0]).FValue.Count;
end;

class procedure TCasScriptXxxArray.HandleArraySetCount(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  NewCount: Int64;
begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  NewCount := TCasScriptInteger(Arguments[1]).Value;
  if NewCount < 0 then
    raise ECasScriptError.CreateFmt('Invalid count %d for array_set_count (should be non-negative)',
      [NewCount]);

  TCasScriptXxxArray(Arguments[0]).FValue.Count := NewCount;
  TCasScriptXxxArray(Arguments[0]).ValueAssigned := true;

  AResult := Arguments[0];
end;

class procedure TCasScriptXxxArray.HandleArrayGet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
  Arr: TXxxList;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptXxxElement);

  Arr := TCasScriptXxxArray(Arguments[0]).Value;

  Index := TCasScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, Arr.Count - 1) then
    raise ECasScriptError.CreateFmt('Invalid index %d for array_get, array count is %d',
      [Index, Arr.Count]);

  TCasScriptXxxElement(AResult).Value := Arr.L[Index];
end;

class procedure TCasScriptXxxArray.HandleArraySet(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
  Arr: TXxxList;
begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  Arr := TCasScriptXxxArray(Arguments[0]).Value;

  Index := TCasScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, Arr.Count - 1) then
    raise ECasScriptError.CreateFmt('Invalid index %d for array_set, array count is %d',
      [Index, Arr.Count]);

  Arr.L[Index] := TCasScriptXxxElement(Arguments[2]).Value;
  TCasScriptXxxArray(Arguments[0]).ValueAssigned := true;

  AResult := Arguments[0];
end;

class procedure TCasScriptXxxArray.HandleAdd(AFunction: TCasScriptFunction; const Arguments: array of TCasScriptValue; var AResult: TCasScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
  Arr: TXxxList;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TCasScriptXxxArray);

  Arr := TCasScriptXxxArray(AResult).Value;
  { initially Arr is empty. This is needed to set explicitly,
    since CreateValueIfNeeded could left previous AResult }
  Arr.Clear;
  for I := 0 to Length(Arguments) - 1 do
    Arr.AddList(TCasScriptXxxArray(Arguments[I]).Value);

  TCasScriptXxxArray(AResult).ValueAssigned := true;
end;

procedure RegisterXxxFunctions;
begin
  FunctionHandlers.RegisterHandler(@TCasScriptXxxArray(nil).HandleArrayFun, TCasScriptXxxArrayFun, [TCasScriptXxxElement], true);
  FunctionHandlers.RegisterHandler(@TCasScriptXxxArray(nil).HandleArrayGetCount, TCasScriptArrayGetCount, [TCasScriptXxxArray], false);
  FunctionHandlers.RegisterHandler(@TCasScriptXxxArray(nil).HandleArraySetCount, TCasScriptArraySetCount, [TCasScriptXxxArray, TCasScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TCasScriptXxxArray(nil).HandleArrayGet, TCasScriptArrayGet, [TCasScriptXxxArray, TCasScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TCasScriptXxxArray(nil).HandleArraySet, TCasScriptArraySet, [TCasScriptXxxArray, TCasScriptInteger, TCasScriptXxxElement], false);
  FunctionHandlers.RegisterHandler(@TCasScriptXxxArray(nil).HandleAdd, TCasScriptAdd, [TCasScriptXxxArray], true);
end;
