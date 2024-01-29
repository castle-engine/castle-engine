{%MainUnit castlecontrols.pas}
{
  Copyright 2017-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { Clipboard to cut / copy / paste the text.
    You usually use this through the single global instance @link(Clipboard).
    The trivial interface is mostly compatible with LCL clipboard. }
  TCastleClipboard = class
  private
    FContents: String;
  protected
    function GetAsText: String; virtual;
    procedure SetAsText(const Value: String); virtual;
  public
    property AsText: String read GetAsText write SetAsText;
  end;

{ Single global instance of TCastleClipboard.
  Automatically created / destroyed by this unit. }
function Clipboard: TCastleClipboard;

{ Register custom TCastleClipboard implementation,
  that replaces the global @link(Clipboard).
  The instance given here becomes owned by this unit (we will free it automatically). }
procedure RegisterClipboard(const AClipboard: TCastleClipboard);

{$endif read_interface}

{$ifdef read_implementation}

var
  FClipboard: TCastleClipboard;

function Clipboard: TCastleClipboard;
begin
  if FClipboard = nil then
    FClipboard := TCastleClipboard.Create;
  Result := FClipboard;
end;

procedure RegisterClipboard(const AClipboard: TCastleClipboard);
begin
  FreeAndNil(FClipboard);
  FClipboard := AClipboard;
end;

procedure FinalizationClipboard;
begin
  FreeAndNil(FClipboard);
end;

{ TCastleClipboard ----------------------------------------------------------- }

{ The default clipboard does not communicate with other processes,
  it's just a trivial container to copy-paste strings
  between Castle Game Engine edit boxes. }

function TCastleClipboard.GetAsText: String;
begin
  Result := FContents;
end;

procedure TCastleClipboard.SetAsText(const Value: String);
begin
  FContents := Value;
end;

{$endif read_implementation}
