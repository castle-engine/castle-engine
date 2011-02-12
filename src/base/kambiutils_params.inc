{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Command-line parameters utilities }

{$ifdef read_interface}

type
  EInvalidParams = class(EWithHiddenClassName);

{ Check is ParamCount equal (or ">=" for ParamCountEqGreater or "<=" for
  for ParamCountEqLesser) to ParamValue.
  @raises(EInvalidParams If the checked condition is not satisfied.)
  @groupBegin }
procedure ParamCountEqual(ParamValue: integer);
procedure ParamCountEqGreater(ParamValue: integer);
procedure ParamCountEqLesser(ParamValue: integer);
{ @groupEnd }

type
  TParametersArray = class(TDynStringArray)
  public
    { Does the number of parameters (High) satisfy given condition.
      @raises EInvalidParams When High is wrong.
      @groupBegin }
    procedure CheckHigh(ParamValue: integer);
    procedure CheckHighAtLeast(ParamValue: integer);
    procedure CheckHighAtMost(ParamValue: integer);
    { @groupEnd }
  end;

var
  { Command-line parameters. Initialized from standard
    ParamStr(0) ... ParamStr(ParamCount). Can be later modified,
    which is good --- you can remove handled parameters.
    You also have all the methods of TDynStringArray class
    (e.g. you can assign to another TDynStringArray instance). }
  Parameters: TParametersArray;

{ Is one of parameters equal to one of SArr.
  Searches in ParamStr(FirstPar) .. ParamStr(LastPar).
  If you don't give FirstPar / LastPar, searches in
  ParamStr(1) .. ParamStr(ParamCount).
  @groupBegin }
function IsPresentInPars(const sarr: array of string;
  IgnoreCase: boolean; FirstPar, LastPar: Cardinal): boolean; overload;
function IsPresentInPars(const sarr: array of string;
  IgnoreCase: boolean): boolean; overload;
{ @groupEnd }

{$endif read_interface}

{$ifdef read_implementation}

function ParametersCountString(Count: Integer; const MiddleStr: string): string; overload;
begin
 result := IntToStr(Count);
 if Count = 1 then
  result := result +MiddleStr +' parameter' else
  result := result +MiddleStr +' parameters';
end;

function ParametersCountString(Count: Integer): string; overload;
begin
 result := ParametersCountString(Count, '');
end;

procedure ParamCountEqual(ParamValue: integer);
begin
 if not (ParamValue = ParamCount) then
  raise EInvalidParams.Create('Expected exactly ' +
    ParametersCountString(ParamValue));
end;

procedure ParamCountEqGreater(ParamValue: integer);
begin
 if not (ParamCount >= ParamValue) then
  raise EInvalidParams.Create('Expected at least ' +
    ParametersCountString(ParamValue));
end;

procedure ParamCountEqLesser(ParamValue: integer);
begin
 if not (ParamCount <= ParamValue) then
  raise EInvalidParams.Create('Expected at most ' +
    ParametersCountString(ParamValue));
end;

function IsPresentInPars(const sarr: array of string;
  IgnoreCase: boolean; FirstPar, LastPar: Cardinal): boolean;
var i, j: integer;
begin
 for i := 1 to ParamCount do
  for j := 0 to High(sarr) do
   if SAnsiSame(ParamStr(i), sarr[j], IgnoreCase) then
    begin result := true; exit end;
 result := false;
end;

function IsPresentInPars(const sarr: array of string;
  IgnoreCase: boolean): boolean;
begin result := IsPresentInPars(sarr, IgnoreCase, 1, Parameters.High) end;

{ Since we can modify Parameters, we can't really output
  in CheckHigh* for user how many parameters were excepted (because you maybe
  ate some). Output only how many params are missing/too much. }

procedure TParametersArray.CheckHigh(ParamValue: integer);
begin
 if ParamValue <> High then
 begin
  if ParamValue < High then
   raise EInvalidParams.Create('Expected ' +
     ParametersCountString(High-ParamValue, ' less')) else
   raise EInvalidParams.Create('Expected ' +
     ParametersCountString(ParamValue-High, ' more'));
 end;
end;

procedure TParametersArray.CheckHighAtLeast(ParamValue: integer);
begin
 if ParamValue > High then
  raise EInvalidParams.Create('Expected ' +
    ParametersCountString(ParamValue-High, ' more'));
end;

procedure TParametersArray.CheckHighAtMost(ParamValue: integer);
begin
  if ParamValue < High then
    raise EInvalidParams.Create('Expected ' +
      ParametersCountString(High-ParamValue, ' less'));
end;

procedure InitializationParams;
var
  I: Integer;
begin
 Parameters := TParametersArray.Create(ParamCount + 1);
 for I := 0 to ParamCount do Parameters[i] := ParamStr(i);
end;

procedure FinalizationParams;
begin
 FreeAndNil(Parameters);
end;

{$endif read_implementation}