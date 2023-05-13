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

{ TCastleRigidBody.LockTranslation and LockRotation property and row editor. }

type

{ TRangeSetPropertyEditor ---------------------------------------------------- }

  { Simple property editor that shows range set value as editor name. }
  TRangeSetPropertyEditor = class (TSetPropertyEditor)
  public
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function OrdValueToVisualValue(OrdValue: Longint): string; override;
  end;

{ T3DCoordsRangeSetPropertyEditor -------------------------------------------- }

  { Property editor designed especially for 3DCoords shows X, Y, Z in
    main property row, and X (0), Y (1), Z (2) in range set element rows }
  T3DCoordsRangeSetPropertyEditor = class(TRangeSetPropertyEditor)
  public
    procedure GetProperties(Proc: TGetPropEditProc); override;
    function OrdValueToVisualValue(OrdValue: Longint): string; override;
  end;

{ TRangeSetElementPropertyEditor --------------------------------------------- }

  { Row editor for TRangeSetPropertyEditor }
  TRangeSetElementPropertyEditor = class (TSetElementPropertyEditor)
  protected
    FElementToShow: Integer;
  public
    constructor Create(Parent: TPropertyEditor; AElement: Integer); reintroduce;
    function GetName: shortstring; override;
  end;

{ T3DCoordsRangeSetElementPropertyEditor ------------------------------------- }

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
