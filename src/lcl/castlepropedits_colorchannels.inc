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

{ Editors for TColorChannels (TCastleRenderOptions.InternalColorChannels property),
  unused now (as property is no longer published). }

type
  { Property editor for TColorChannels }
  TColorChannelsRangeSetPropertyEditor = class(TRangeSetPropertyEditor)
  public
    procedure GetProperties(Proc: TGetPropEditProc); override;
  end;

  { Row editor for TColorChannels }
  TColorChannelsRangeSetElementPropertyEditor = class (TRangeSetElementPropertyEditor)
    function GetName: shortstring; override;
  end;

{ TColorChannelsRangeSetPropertyEditor -------------------------------------------- }

procedure TColorChannelsRangeSetPropertyEditor.GetProperties(Proc: TGetPropEditProc);
var
  I: Integer;
begin
  with GetTypeData(GetTypeData(GetPropType)^.CompType)^ do
    for I := MinValue to MaxValue do
      Proc(TColorChannelsRangeSetElementPropertyEditor.Create(Self, I));
end;

{ TColorChannelsRangeSetElementPropertyEditor ------------------------------------- }

function TColorChannelsRangeSetElementPropertyEditor.GetName: shortstring;
begin
  case FElementToShow of
    // Write digit first, as these strings are sorted alphabetically in Object Inspector
    0: Result := '0 - Red';
    1: Result := '1 - Green';
    2: Result := '2 - Blue';
    3: Result := '3 - Alpha';
    else
      Result := IntToStr(FElementToShow);
  end;
end;
