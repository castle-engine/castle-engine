{
  Copyright 2002-2006 Michalis Kamburelis.

  This file is part of "Kambi's base Pascal units".

  "Kambi's base Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's base Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's base Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
    { sprawdza  czy High [= >= <=] ParamValue. Jesli nie, wyjatek EInvalidParams
      z odpowiednim komunikatem bledu. (niestety, kara za mozliwosc
      modyfikacji parametrow przechowywanych w obiektach tej klasy
      jest fakt ze te funkcje nie moga wypisac dokladnie ILE parametrow oczekiwano,
      bo przeciez ilosc parametrow mogla ulec zmianie od czasu rozpoczecia
      programu. Moga jedynie wypisac ILE parametrow brakuje/jest za duzo. }
    procedure CheckHigh(ParamValue: integer);
    procedure CheckHighAtLeast(ParamValue: integer);
    procedure CheckHighAtMost(ParamValue: integer);

    { Deletes item I, checks whether I is still <= High,
      returns Parameters[I].
      @raises(EInvalidParams If there is no Items[I] after previous item
        I was deleted.) }
    function DeleteAndGetNext(I: Integer): string;

    { Assign Parameters value from Argc and Argv.
      This is extremely useful if you use libraries that
      require Argc and Argv and modify passed Argc and Argv.
      (Like glutInit, MPI_Init, and so on).
      Then you can load Parameters again from resulting Argc and Argv. }
    procedure AssignArgCV(Argc: Integer; Argv: PPChar);
  end;

var
  { At the initialization of this unit, Parameters are initialized to
    simply ParamStr(0) ... ParamStr(ParamCount). This means that
    Parameters[I] is simply equivalent to ParamStr(I) and
    Parameters.High is simply equivalent to ParamCount.

    However Parameters give you more possibilities:
    @unorderedList(
      @item(You have all the comfortable methods of TDynStringArray.
        E.g. copying to other instance of TDynStringArray is now trivially
        easy --- just use TDynStringArray.Assign.)

      @item(You can modify contents of Parameters. This is very useful for
        routines that parse command-line paramaters (like my unit
        ParseParametersUnit) --- they can simply remove from Paramaters
        the items that are already handled.

        This is similar to the idea how glut and many other C libraries
        modify passed argv --- they remove parts of command-line that
        were already parsed.)
    )

    @noAutoLinkHere
  }
  Parameters: TParametersArray;

{ czy ktorys z parametrow ParStr(FirstPar) .. ParStr(LastPar)
  jest rowny ktoremus z sarr ? Jezeli nie podasz FirstPar i LastPar
  to zostana przyjete jako 1, ParCount. }
function IsPresentInPars(const sarr: array of string;
  IgnoreCase: boolean; FirstPar, LastPar: Cardinal): boolean; overload;
function IsPresentInPars(const sarr: array of string;
  IgnoreCase: boolean): boolean; overload;

{ IsHelpPresentInPars = IsPresentInPars(['-?', '-h', '--help'], false) }
function IsHelpPresentInPars: boolean;

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

function IsHelpPresentInPars: boolean;
const HelpPars: array[0..2]of string = ('-?', '-h', '--help');
begin result := IsPresentInPars(HelpPars, false) end;

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

procedure TParametersArray.AssignArgCV(Argc: Integer; Argv: PPChar);
var
  Argv_Array: PArray_PChar absolute Argv;
  I: Integer;
begin
  Count := Argc;
  for I := 0 to Count - 1 do
    Items[I] := Argv_Array[I];
end;

function TParametersArray.DeleteAndGetNext(I: Integer): string;
begin
  Delete(I, 1);
  if I > High then raise EInvalidParams.Create('Unexpected end of params');
  Result := Parameters[I];
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