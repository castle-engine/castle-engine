{
  Copyright 2018-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Display instructions. }
unit GameViewWin;

interface

uses CastleControls, CastleWindow, CastleUIControls,
  CastleKeysMouse;

type
  TViewWin = class(TCastleView)
  public
    HumansWin: Boolean; //< Set this before starting this view.
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewWin: TViewWin;

implementation

uses SysUtils, Classes,
  CastleComponentSerialize,
  GameViewMainMenu;

procedure TViewWin.Start;
begin
  inherited;

  { Load designed user interface }
  if HumansWin then
    DesignUrl := 'castle-data:/gameviewwinhumans.castle-user-interface'
  else
    DesignUrl := 'castle-data:/gameviewwinaliens.castle-user-interface';

  { do not pass clicks to view underneath }
  InterceptInput := true;
end;

function TViewWin.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;

  if Event.IsMouseButton(buttonLeft) then
  begin
    Container.View := ViewMainMenu;
    Exit(true);
  end;
end;

end.
