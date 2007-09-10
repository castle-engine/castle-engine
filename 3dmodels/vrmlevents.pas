{
  Copyright 2007 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ }
unit VRMLEvents;

interface

uses SysUtils, KambiUtils, KambiClassUtils, VRMLFields, VRMLLexer;

{$define read_interface}

type
  { VRML event.

    This is really dummy thing for now, used only to parse and store
    parsed events information.
    We don't do nothing more with VRML events for now... }
  TVRMLEvent = class
  private
    FName: string;
    FFieldClass: TVRMLFieldClass;
    FInEvent: boolean;
    FIsClause: boolean;
    FIsClauseName: string;
  public
    constructor Create(const AName: string;
      const AFieldClass: TVRMLFieldClass; const AInEvent: boolean);

    property Name: string read FName;
    property FieldClass: TVRMLFieldClass read FFieldClass;

    property IsClause: boolean read FIsClause;
    property IsClauseName: string read FIsClauseName;

    { @abstract(Is it "in" or "out" event ?) }
    property InEvent: boolean read FInEvent;

    { This only reads (optional) "IS" clause of the event, as may occur
      in VRML nodeBodyStatement. }
    procedure Parse(Lexer: TVRMLLexer);
  end;

  TObjectsListItem_1 = TVRMLEvent;
  {$I objectslist_1.inc}
  TVRMLEventsList = class(TObjectsList_1)
  public
    function IndexOf(const Name: string): Integer;
  end;

{$undef read_interface}

implementation

{$define read_implementation}
{$I objectslist_1.inc}

constructor TVRMLEvent.Create(
  const AName: string; const AFieldClass: TVRMLFieldClass;
  const AInEvent: boolean);
begin
  inherited Create;
  FName := AName;
  FFieldClass := AFieldClass;
  FInEvent := AInEvent;
end;

procedure TVRMLEvent.Parse(Lexer: TVRMLLexer);
begin
  FIsClause := Lexer.TokenIsKeyword(vkIS);
  if FIsClause then
  begin
    Lexer.NextToken;
    FIsClauseName := Lexer.TokenName;
    Lexer.NextToken;
  end;
end;

{ TVRMLEventsList ------------------------------------------------------------ }

function TVRMLEventsList.IndexOf(const Name: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Name = Name then Exit;
  Result := -1;
end;

end.
