{
  Copyright 2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  ----------------------------------------------------------------------------
}

constructor TKamScriptXxxArray.Create(const AWriteable: boolean; const AValue: TDynXxxArray);
begin
  Create(AWriteable);
  Value := AValue;
end;

constructor TKamScriptXxxArray.Create(const AWriteable: boolean);
begin
  inherited;
  FValue := TDynXxxArray.Create;
end;

destructor TKamScriptXxxArray.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;

procedure TKamScriptXxxArray.SetValue(const AValue: TDynXxxArray);
begin
  FValue.Assign(AValue);
  ValueAssigned := true;
end;

procedure TKamScriptXxxArray.AssignValue(Source: TKamScriptValue);
begin
  if Source is TKamScriptXxxArray then
    Value := TKamScriptXxxArray(Source).Value else
    raise EKamAssignValueError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

class procedure TKamScriptXxxArray.HandleArrayFun(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
  Arr: TDynXxxArray;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptXxxArray);

  Arr := TKamScriptXxxArray(AResult).Value;
  Arr.Count := Length(Arguments);
  for I := 0 to Length(Arguments) - 1 do
    Arr.Items[I] := TKamScriptXxxElement(Arguments[I]).Value;

  TKamScriptXxxArray(AResult).ValueAssigned := true;
end;

class procedure TKamScriptXxxArray.HandleArrayGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value :=
    TKamScriptXxxArray(Arguments[0]).FValue.Count;
end;

class procedure TKamScriptXxxArray.HandleArraySetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  NewCount: Int64;
begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  NewCount := TKamScriptInteger(Arguments[1]).Value;
  if NewCount < 0 then
    raise EKamScriptError.CreateFmt('Invalid count %d for array_set_count (should be non-negative)',
      [NewCount]);

  TKamScriptXxxArray(Arguments[0]).FValue.Count := NewCount;
  TKamScriptXxxArray(Arguments[0]).ValueAssigned := true;

  AResult := Arguments[0];
end;

class procedure TKamScriptXxxArray.HandleArrayGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
  Arr: TDynXxxArray;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptXxxElement);

  Arr := TKamScriptXxxArray(Arguments[0]).Value;

  Index := TKamScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, Arr.Count - 1) then
    raise EKamScriptError.CreateFmt('Invalid index %d for array_get, array count is %d',
      [Index, Arr.Count]);

  TKamScriptXxxElement(AResult).Value := Arr.Items[Index];
end;

class procedure TKamScriptXxxArray.HandleArraySet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  Index: Integer;
  Arr: TDynXxxArray;
begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  Arr := TKamScriptXxxArray(Arguments[0]).Value;

  Index := TKamScriptInteger(Arguments[1]).Value;
  if not Between(Index, 0, Arr.Count - 1) then
    raise EKamScriptError.CreateFmt('Invalid index %d for array_set, array count is %d',
      [Index, Arr.Count]);

  Arr.Items[Index] := TKamScriptXxxElement(Arguments[2]).Value;
  TKamScriptXxxArray(Arguments[0]).ValueAssigned := true;

  AResult := Arguments[0];
end;

procedure RegisterXxxFunctions;
begin
  FunctionHandlers.RegisterHandler(@TKamScriptXxxArray(nil).HandleArrayFun, TKamScriptXxxArrayFun, [TKamScriptXxxElement], true);
  FunctionHandlers.RegisterHandler(@TKamScriptXxxArray(nil).HandleArrayGetCount, TKamScriptArrayGetCount, [TKamScriptXxxArray], false);
  FunctionHandlers.RegisterHandler(@TKamScriptXxxArray(nil).HandleArraySetCount, TKamScriptArraySetCount, [TKamScriptXxxArray, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptXxxArray(nil).HandleArrayGet, TKamScriptArrayGet, [TKamScriptXxxArray, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptXxxArray(nil).HandleArraySet, TKamScriptArraySet, [TKamScriptXxxArray, TKamScriptInteger, TKamScriptXxxElement], false);
end;
