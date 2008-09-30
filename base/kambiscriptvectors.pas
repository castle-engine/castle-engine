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
}

{ KambiScript vector and matrix types and built-in functions. }
unit KambiScriptVectors;

interface

uses VectorMath, KambiScript;

type
  TKamScriptVec2f = class(TKamScriptValue)
  private
    class procedure HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    class procedure HandleVector(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorSqrLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
    class procedure HandleVectorDot(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
//    class procedure HandleVectorCross(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);

    FValue: TVector2Single;
    procedure SetValue(const AValue: TVector2Single);
  public
    property Value: TVector2Single read FValue write SetValue;

    procedure AssignValue(Source: TKamScriptValue); override;
  end;

  TKamScriptVector = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptVectorGet = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptVectorSet = class(TKamScriptFunction)
  protected
    procedure CheckArguments; override;
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptVectorGetCount = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptVectorLength = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptVectorSqrLength = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptVectorDot = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

  TKamScriptVectorCross = class(TKamScriptFunction)
  public
    class function Name: string; override;
    class function ShortName: string; override;
  end;

implementation

uses KambiScriptCoreFunctions;

{ TKamScriptVec2f ---------------------------------------------------------- }

class procedure TKamScriptVec2f.HandleAdd(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVec2f);
  { The function allows only >= 1 arguments, and this handler is
    registered only for TKamScriptVec2f values, so we can safely take
    the first arg as TKamScriptVec2f. }
  TKamScriptVec2f(AResult).Value := TKamScriptVec2f(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptVec2f(AResult).Value := VectorAdd(
      TKamScriptVec2f(AResult).Value, TKamScriptVec2f(Arguments[I]).Value);
end;

class procedure TKamScriptVec2f.HandleSubtract(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
var
  I: Integer;
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVec2f);
  TKamScriptVec2f(AResult).Value := TKamScriptVec2f(Arguments[0]).Value;
  for I := 1 to Length(Arguments) - 1 do
    TKamScriptVec2f(AResult).Value := VectorSubtract(
      TKamScriptVec2f(AResult).Value, TKamScriptVec2f(Arguments[I]).Value);
end;

class procedure TKamScriptVec2f.HandleNegate(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVec2f);
  TKamScriptVec2f(AResult).Value := VectorNegate(TKamScriptVec2f(Arguments[0]).Value);
end;

class procedure TKamScriptVec2f.HandleEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    VectorsPerfectlyEqual(
      TKamScriptVec2f(Arguments[0]).Value,
      TKamScriptVec2f(Arguments[1]).Value);
end;

class procedure TKamScriptVec2f.HandleNotEqual(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptBoolean);
  TKamScriptBoolean(AResult).Value :=
    not VectorsPerfectlyEqual(
      TKamScriptVec2f(Arguments[0]).Value,
      TKamScriptVec2f(Arguments[1]).Value);
end;

class procedure TKamScriptVec2f.HandleVector(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVec2f);
  TKamScriptVec2f(AResult).FValue[0] := TKamScriptFloat(Arguments[0]).Value;
  TKamScriptVec2f(AResult).FValue[1] := TKamScriptFloat(Arguments[1]).Value;
  TKamScriptVec2f(AResult).ValueAssigned := true;
end;

class procedure TKamScriptVec2f.HandleVectorGet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value :=
    TKamScriptVec2f(Arguments[0]).Value[TKamScriptInteger(Arguments[1]).Value];
end;

class procedure TKamScriptVec2f.HandleVectorSet(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  if ParentOfResult then
    AResult.FreeByParentExpression;
  AResult := nil;
  ParentOfResult := false;

  TKamScriptVec2f(Arguments[0]).FValue[TKamScriptInteger(Arguments[1]).Value] :=
    TKamScriptFloat(Arguments[2]).Value;
  TKamScriptVec2f(Arguments[0]).ValueAssigned := true;

  AResult := Arguments[0];
end;

class procedure TKamScriptVec2f.HandleVectorGetCount(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptInteger);
  TKamScriptInteger(AResult).Value := 2;
end;

class procedure TKamScriptVec2f.HandleVectorLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value :=
    VectorLen(TKamScriptVec2f(Arguments[0]).Value);
end;

class procedure TKamScriptVec2f.HandleVectorSqrLength(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value :=
    VectorLenSqr(TKamScriptVec2f(Arguments[0]).Value);
end;

class procedure TKamScriptVec2f.HandleVectorDot(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptFloat);
  TKamScriptFloat(AResult).Value :=
    VectorDotProduct( TKamScriptVec2f(Arguments[0]).Value,
                      TKamScriptVec2f(Arguments[1]).Value );
end;

procedure TKamScriptVec2f.AssignValue(Source: TKamScriptValue);
begin
  if Source is TKamScriptVec2f then
    Value := TKamScriptVec2f(Source).Value else
    raise EKamAssignValueError.CreateFmt('Assignment from %s to %s not possible', [Source.ClassName, ClassName]);
end;

procedure TKamScriptVec2f.SetValue(const AValue: TVector2Single);
begin
  FValue := AValue;
  ValueAssigned := true;
end;

{

class procedure HandleVectorCross(AFunction: TKamScriptFunction; const Arguments: array of TKamScriptValue; var AResult: TKamScriptValue; var ParentOfResult: boolean);
begin
  CreateValueIfNeeded(AResult, ParentOfResult, TKamScriptVec3f);
  TKamScriptVec3f(AResult).Value :=
    VectorProduct( TKamScriptVec3f(Arguments[0]).Value,
                   TKamScriptVec3f(Arguments[1]).Value );
end;}

{ TKamScriptFunction descendants --------------------------------------------- }

class function TKamScriptVector.Name: string;
begin
  Result := 'vector';
end;

class function TKamScriptVector.ShortName: string;
begin
  Result := 'vector';
end;

class function TKamScriptVectorGet.Name: string;
begin
  Result := 'vector_get';
end;

class function TKamScriptVectorGet.ShortName: string;
begin
  Result := 'vector_get';
end;

class function TKamScriptVectorSet.Name: string;
begin
  Result := 'vector_set';
end;

class function TKamScriptVectorSet.ShortName: string;
begin
  Result := 'vector_set';
end;

procedure TKamScriptVectorSet.CheckArguments;
begin
  inherited;
  if not ( (Args[0] is TKamScriptValue) and
           TKamScriptValue(Args[0]).Writeable ) then
    raise EKamScriptFunctionArgumentsError.Create('First argument of "vector_set" function is not a writeable operand');
end;

class function TKamScriptVectorGetCount.Name: string;
begin
  Result := 'vector_get_count';
end;

class function TKamScriptVectorGetCount.ShortName: string;
begin
  Result := 'vector_get_count';
end;

class function TKamScriptVectorLength.Name: string;
begin
  Result := 'vector_length';
end;

class function TKamScriptVectorLength.ShortName: string;
begin
  Result := 'vector_length';
end;

class function TKamScriptVectorSqrLength.Name: string;
begin
  Result := 'vector_sqr_length';
end;

class function TKamScriptVectorSqrLength.ShortName: string;
begin
  Result := 'vector_sqr_length';
end;

class function TKamScriptVectorDot.Name: string;
begin
  Result := 'vector_dot';
end;

class function TKamScriptVectorDot.ShortName: string;
begin
  Result := 'vector_dot';
end;

class function TKamScriptVectorCross.Name: string;
begin
  Result := 'vector_cross';
end;

class function TKamScriptVectorCross.ShortName: string;
begin
  Result := 'vector_cross';
end;

{ unit init/fini ------------------------------------------------------------- }

initialization
  { TKamScriptVec2f, functions from KambiScriptCoreFunctions }
  FunctionHandlers.RegisterHandler(@TKamScriptVec2f(nil).HandleAdd, TKamScriptAdd, [TKamScriptVec2f], true);
  FunctionHandlers.RegisterHandler(@TKamScriptVec2f(nil).HandleSubtract, TKamScriptSubtract, [TKamScriptVec2f], true);
  FunctionHandlers.RegisterHandler(@TKamScriptVec2f(nil).HandleNegate, TKamScriptNegate, [TKamScriptVec2f], false);
  FunctionHandlers.RegisterHandler(@TKamScriptVec2f(nil).HandleEqual, TKamScriptEqual, [TKamScriptVec2f, TKamScriptVec2f], false);
  FunctionHandlers.RegisterHandler(@TKamScriptVec2f(nil).HandleNotEqual, TKamScriptNotEqual, [TKamScriptVec2f, TKamScriptVec2f], false);

  { TKamScriptVec2f, functions from KambiScriptVectors }
  FunctionHandlers.RegisterHandler(@TKamScriptVec2f(nil).HandleVector, TKamScriptVector, [TKamScriptFloat, TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptVec2f(nil).HandleVectorGet, TKamScriptVectorGet, [TKamScriptVec2f, TKamScriptInteger], false);
  FunctionHandlers.RegisterHandler(@TKamScriptVec2f(nil).HandleVectorSet, TKamScriptVectorSet, [TKamScriptVec2f, TKamScriptInteger, TKamScriptFloat], false);
  FunctionHandlers.RegisterHandler(@TKamScriptVec2f(nil).HandleVectorGetCount, TKamScriptVectorGetCount, [TKamScriptVec2f], false);
  FunctionHandlers.RegisterHandler(@TKamScriptVec2f(nil).HandleVectorLength, TKamScriptVectorLength, [TKamScriptVec2f], false);
  FunctionHandlers.RegisterHandler(@TKamScriptVec2f(nil).HandleVectorSqrLength, TKamScriptVectorSqrLength, [TKamScriptVec2f], false);
  FunctionHandlers.RegisterHandler(@TKamScriptVec2f(nil).HandleVectorDot, TKamScriptVectorDot, [TKamScriptVec2f, TKamScriptVec2f], false);


//  FunctionHandlers.RegisterHandler(@TKamScriptVec3f(nil).HandleVectorCross, TKamScriptVectorCross, [TKamScriptVec3f, TKamScriptVec3f], false);
end.
