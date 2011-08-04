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
  { List of strings. This is a slightly extended version of standard TStringList.
    The default CaseSensitive value is @true. }
  TKamStringList = class(TStringList)
  private
    procedure SetCount(const Value: Integer);
  public
    constructor Create;
    function High: Integer;
    property Count: Integer read GetCount write SetCount;
    { Add strings from Source list.
      Alias for AddStrings, useful for kambiscriptarrays_implement.inc
      (since it's consistent with AddList in other lists). }
    procedure AddList(const Source: TStringList);
    procedure AddArray(const A: array of string);
    procedure AssignArray(const A: array of string);
    function Equal(List: TKamStringList): boolean; overload;
    function Equal(const A: array of string): boolean; overload;
    { Reverse the order of items on the array. }
    procedure Reverse;
  end;

  TParametersArray = class(TKamStringList)
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
    You also have all the methods of TKamStringList class
    (e.g. you can assign to another TKamStringList instance). }
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

constructor TKamStringList.Create;
begin
  inherited;
  CaseSensitive := true;
end;

procedure TKamStringList.SetCount(const Value: Integer);
var
  I: Integer;
begin
  { Use local variable I, instead of comparing Value = Count for,
    to possibly speed up a little (GetCount is virtual) }
  if Value < Count then
  begin
    for I := 1 to Count - Value do Delete(Count - 1);
  end else
  if Value > Count then
  begin
    for I := 1 to Value - Count do Add('');
  end;
end;

function TKamStringList.High: Integer;
begin
  Result := Count - 1;
end;

procedure TKamStringList.AddList(const Source: TStringList);
begin
  AddStrings(Source);
end;

procedure TKamStringList.AddArray(const A: array of string);
var
  I: Integer;
begin
  for I := 0 to System.High(A) do
    Add(A[I]);
end;

procedure TKamStringList.AssignArray(const A: array of string);
begin
  Clear;
  AddArray(A);
end;

procedure TKamStringList.Reverse;
var
  I: Integer;
begin
  { Need to specially check for Count = 0 case, since (0-1) div 2 = -1 div 2 = 0
    which means that loop would try invalid SwapItems(0, -1). }
  if Count = 0 then Exit;
  for I := 0 to (Count - 1) div 2 do
    Exchange(I, Count - 1 - I);
end;

function TKamStringList.Equal(List: TKamStringList): boolean;
var
  I: Integer;
begin
  if List.Count <> Count then Exit(false);
  for I := 0 to Count - 1 do
    if DoCompareText(List[I], Strings[I]) <> 0 then
      Exit(false);
  Result := true;
end;

function TKamStringList.Equal(const A: array of string): boolean;
var
  I: Integer;
begin
  if System.High(A) <> High then Exit(false);
  for I := 0 to Count - 1 do
    if DoCompareText(A[I], Strings[I]) <> 0 then
      Exit(false);
  Result := true;
end;

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
 Parameters := TParametersArray.Create;
 for I := 0 to ParamCount do
   Parameters.Add(ParamStr(i));
end;

procedure FinalizationParams;
begin
 FreeAndNil(Parameters);
end;

{$endif read_implementation}