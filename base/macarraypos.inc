{
  Copyright 2003-2005 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ first define ARRAY_POS_FUNCTION_NAME,
    ARRAY_POS_ARRAY_NAME,
    ARRAY_POS_INDEX_TYPE. }

{ ------------------------------------------------------------
  Macro IMPLEMENT_ARRAY_POS will define function
  that seeks s in array ARRAY_POS_ARRAY_NAME (case-sensitive).
  It returns true ans sets "Index" appropriately if s is found,
  it returns false (and doesn't touch "Index") if s is not found.
  Note : this function is intended to be a replacement for
    ArrayPosStr in KambiUtils.
    Function defined here will be type-safe. }

{$define IMPLEMENT_ARRAY_POS :=
function ARRAY_POS_FUNCTION_NAME(const s: string;
  var Index: ARRAY_POS_INDEX_TYPE): boolean;
var i: ARRAY_POS_INDEX_TYPE;
begin
 for i := Low(ARRAY_POS_ARRAY_NAME) to High(ARRAY_POS_ARRAY_NAME) do
  if ARRAY_POS_ARRAY_NAME[i] = s then
  begin
   Index := i;
   exit(true);
  end;
 result := false;
end;}

{ ------------------------------------------------------------
  Macro IMPLEMENT_ARRAY_POS_CHECKED will define function
  that seeks s in array ARRAY_POS_ARRAY_NAME (case-sensitive).
  It returns Index found. If s in not one of ARRAY_POS_ARRAY_NAME[]
  then exception is raised. }

{$define IMPLEMENT_ARRAY_POS_CHECKED :=
function ARRAY_POS_FUNCTION_NAME(const s: string): ARRAY_POS_INDEX_TYPE;
var i: ARRAY_POS_INDEX_TYPE;
begin
 for i := Low(ARRAY_POS_ARRAY_NAME) to High(ARRAY_POS_ARRAY_NAME) do
  if ARRAY_POS_ARRAY_NAME[i] = s then
   begin result := i; Exit; end;
 raise Exception.Create('"'+s+'" does not match any of the allowed values');
end;}

{ ------------------------------------------------------------
  Same as IMPLEMENT_ARRAY_POS_CHECKED, but this time we have a parameter
  IgnoreCase: boolean }

{$define IMPLEMENT_ARRAY_POS_CASE_CHECKED :=
function ARRAY_POS_FUNCTION_NAME(const s: string; IgnoreCase: boolean):
  ARRAY_POS_INDEX_TYPE;
var i: ARRAY_POS_INDEX_TYPE;
begin
 if IgnoreCase then
 begin
  for i := Low(ARRAY_POS_ARRAY_NAME) to High(ARRAY_POS_ARRAY_NAME) do
   if AnsiSameText(ARRAY_POS_ARRAY_NAME[i], s) then
    begin result := i; Exit; end;
 end else
 begin
  for i := Low(ARRAY_POS_ARRAY_NAME) to High(ARRAY_POS_ARRAY_NAME) do
   if ARRAY_POS_ARRAY_NAME[i] = s then
    begin result := i; Exit; end;
 end;
 raise Exception.Create('"'+s+'" does not match any of the allowed values');
end;}

{ ------------------------------------------------------------
  "dirty" version of IMPLEMENT_ARRAY_POS that returns Integer that can
  be casted to ARRAY_POS_INDEX_TYPE if needed. }

{$define IMPLEMENT_ARRAY_POS_INT :=
function ARRAY_POS_FUNCTION_NAME(const s: string): integer;
var i: ARRAY_POS_INDEX_TYPE;
begin
 for i := Low(ARRAY_POS_ARRAY_NAME) to High(ARRAY_POS_ARRAY_NAME) do
  if ARRAY_POS_ARRAY_NAME[i] = s then exit(Ord(i));
 result := -1;
end;}
