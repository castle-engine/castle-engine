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

{ Editor for sets of integers. }

type
  { Simple property editor that shows range set value as editor name. }
  TRangeSetPropertyEditor = class (TSetPropertyEditor)
  public
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function OrdValueToVisualValue(OrdValue: Longint): string; override;
  end;

  { Row editor for TRangeSetPropertyEditor }
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

{ Note: This is unused for now.
  All actual usage of TRangeSetPropertyEditor is through T3DCoordsRangeSetPropertyEditor,
  that overrides this. }

var
  S: TIntegerSet;
  I: Integer;
begin
  Integer(S) := OrdValue;
  Result := '';
  for I := 0 to SizeOf(Integer) * 8 - 1 do
    if I in S then
      Result := SAppendPart(Result, ',', IntToStr(I));
  Result := '[' + Result + ']';
end;

{ TRangeSetElementPropertyEditor ---------------------------------------------------- }

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
