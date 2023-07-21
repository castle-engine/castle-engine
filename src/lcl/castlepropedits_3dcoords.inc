{%MainUnit castlepropedits.pas}
{
  Copyright 2010-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Editors for T3DCoords (TCastleRigidBody.LockTranslation, LockRotation properties). }

type
  { Property editor designed especially for 3DCoords shows X, Y, Z in
    main property row, and X (0), Y (1), Z (2) in range set element rows }
  T3DCoordsRangeSetPropertyEditor = class(TRangeSetPropertyEditor)
  public
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function OrdValueToVisualValue(OrdValue: Longint): string; override;
  end;

  { Row editor for T3DCoordsRangeSetPropertyEditor }
  T3DCoordsRangeSetElementPropertyEditor = class (TRangeSetElementPropertyEditor)
    function GetName: shortstring; override;
  end;

{ T3DCoordsRangeSetPropertyEditor -------------------------------------------- }

procedure T3DCoordsRangeSetPropertyEditor.GetProperties(Proc: TGetPropEditProc);
var
  I: Integer;
begin
  with GetTypeData(GetTypeData(GetPropType)^.CompType)^ do
    for I := MinValue to MaxValue do
      Proc(T3DCoordsRangeSetElementPropertyEditor.Create(Self, I));
end;

function T3DCoordsRangeSetPropertyEditor.OrdValueToVisualValue(
  OrdValue: Longint): string;
var
  S: TIntegerSet;
  I: Integer;
begin
  Integer(S) := OrdValue;
  Result := '';
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      case I of
        0: Result := SAppendPart(Result, ',', 'X');
        1: Result := SAppendPart(Result, ',', 'Y');
        2: Result := SAppendPart(Result, ',', 'Z');
        else
           Result := SAppendPart(Result, ',', IntToStr(I));
      end;
    end;
  Result := '[' + Result + ']';
end;

{ T3DCoordsRangeSetElementPropertyEditor ------------------------------------- }

function T3DCoordsRangeSetElementPropertyEditor.GetName: shortstring;
begin
  case FElementToShow of
    0: Result := 'X (0)';
    1: Result := 'Y (1)';
    2: Result := 'Z (2)';
    else
      Result := IntToStr(FElementToShow);
  end;
end;
