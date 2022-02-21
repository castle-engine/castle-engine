{%MainUnit castlepropedits.pas}
{
  Copyright 2010-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TCastleRigidBody.LockTranslation and LockRotation property and row editor. }

type

{ TRangeSetPropertyEditor ---------------------------------------------------- }

  { Property editor to select values of T3DCoords set. }
  TRangeSetPropertyEditor = class (TSetPropertyEditor)
  public
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function OrdValueToVisualValue(OrdValue: Longint): string; override;
  end;

{ TRangeSetElementPropertyEditor --------------------------------------------- }

  { Row editor TRangeSetPropertyEditor }
  TRangeSetElementPropertyEditor = class (TSetElementPropertyEditor)
  protected
    FElementToShow: Integer;
  public
    constructor Create(Parent: TPropertyEditor; AElement: Integer); reintroduce;
    function GetName: shortstring; override;
  end;


{ TRangeSetPropertyEditor ---------------------------------------------------- }

procedure TRangeSetPropertyEditor.GetProperties(Proc: TGetPropEditProc);
var
  I: Integer;
begin
  with GetTypeData(GetTypeData(GetPropType)^.CompType)^ do
    for I := MinValue to MaxValue do
      Proc(TRangeSetElementPropertyEditor.Create(Self, I));
end;

function TRangeSetPropertyEditor.OrdValueToVisualValue(OrdValue: Longint): String;
var
  S: TIntegerSet;
  I: Integer;
begin
  Integer(S) := OrdValue;
  Result := '[';
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
    begin
      if Length(Result) <> 1 then
        Result := Result + ',';
      Result := Result + IntToStr(I);
    end;
  Result := Result + ']';
end;


{ TRangeSetPropertyEditor ---------------------------------------------------- }

constructor TRangeSetElementPropertyEditor.Create(Parent: TPropertyEditor;
  AElement: Integer);
begin
  inherited Create(Parent, AElement);
  FElementToShow := AElement;
end;

function TRangeSetElementPropertyEditor.GetName: shortstring;
begin
  Result := IntToStr(FElementToShow);
end;

